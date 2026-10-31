module Main (main) where

import Control.Exception (IOException, evaluate, try)
import Control.Monad (when)
import Data.Char (toLower)
import Data.List (isInfixOf, sort)
import Data.Maybe (fromMaybe)
import System.Directory (copyFile, listDirectory)
import System.Environment (getArgs)
import System.Exit
import System.FilePath (takeBaseName, takeExtension)
import System.IO
import System.Process
import Text.Read (readMaybe)

import FixCabalDeps.Edit
import FixCabalDeps.Parse
import FixCabalDeps.Types

------------------------------------------------------------------------------
-- Options

data Opts = Opts
  { oFile :: Maybe FilePath
  , oMax :: Int
  , oDry :: Bool
  , oBackup :: Bool
  , oCabalArgs :: [String]
  }

defaultCabalArgs :: [String]
defaultCabalArgs = ["build", "all", "--enable-tests", "--enable-benchmarks", "--keep-going"]

usage :: String
usage =
  unlines
    [ "cabal-autodeps - run `cabal build` until no dependency is missing"
    , ""
    , "Usage: cabal-autodeps [OPTIONS] [-- CABAL_ARGS...]"
    , ""
    , "  --cabal-file FILE     the .cabal file to edit (default: the one in the current dir)"
    , "  --max-iterations N    give up after N builds (default: 50)"
    , "  --dry-run             build once, show what would be added, change nothing"
    , "  --no-backup           do not write FILE.bak before editing"
    , "  -h, --help            show this help"
    , "  -- CABAL_ARGS         replace the default cabal arguments, which are:"
    , "                        " ++ unwords defaultCabalArgs
    ]

parseArgs :: [String] -> Either String (Maybe Opts)
parseArgs = go (Opts Nothing 50 False True defaultCabalArgs)
  where
    go o [] = Right (Just o)
    go _ ("-h" : _) = Right Nothing
    go _ ("--help" : _) = Right Nothing
    go o ("--cabal-file" : f : r) = go o {oFile = Just f} r
    go o ("--max-iterations" : n : r) = case readMaybe n of
      Just k | k > 0 -> go o {oMax = k} r
      _ -> Left ("invalid --max-iterations value: " ++ n)
    go o ("--dry-run" : r) = go o {oDry = True} r
    go o ("--no-backup" : r) = go o {oBackup = False} r
    go o ("--" : r) = Right (Just o {oCabalArgs = r})
    go _ (x : _) = Left ("unknown argument: " ++ x)

------------------------------------------------------------------------------
-- Main loop

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  args <- getArgs
  case parseArgs args of
    Left e -> hPutStrLn stderr e >> hPutStr stderr usage >> exitWith (ExitFailure 2)
    Right Nothing -> putStr usage
    Right (Just opts) -> do
      file <- maybe locateCabalFile return (oFile opts)
      when (oBackup opts && not (oDry opts)) $ do
        copyFile file (file ++ ".bak")
        putStrLn ("Backup written to " ++ file ++ ".bak")
      loop opts file 1

loop :: Opts -> FilePath -> Int -> IO ()
loop opts file i
  | i > oMax opts = failWith ("giving up after " ++ show (oMax opts) ++ " iterations")
  | otherwise = do
      putStrLn ("\n=== Iteration " ++ show i ++ ": cabal " ++ unwords (oCabalArgs opts) ++ " ===")
      (code, out) <- runCabal (oCabalArgs opts)
      case code of
        ExitSuccess -> do
          putStrLn "\nBuild succeeded: no more missing dependencies."
          exitSuccess
        ExitFailure _ -> do
          let an = analyse out
          when (aSolverFailure an) $
            failWith
              "cabal could not resolve dependencies (see output above). A package that was just \
              \added may not exist, or its bounds conflict; run `cabal update` or fix the cabal file by hand."
          when (aCabalFileError an) $
            failWith
              ( "cabal cannot parse " ++ file ++ " (see output above). "
                  ++ "Restore the backup with: cp " ++ file ++ ".bak " ++ file
              )
          (ls, eol) <- readCabal file
          let pname = fromMaybe (takeBaseName file) (packageName ls)
              (ls', outcomes) = applyAll pname ls (aFindings an)
              changed = ls' /= ls
          putStrLn ""
          mapM_ (putStrLn . render) outcomes
          mapM_ reportUnresolved (aUnresolved an)
          if oDry opts
            then putStrLn "\n--dry-run: nothing written." >> exitSuccess
            else
              if changed
                then writeCabal file eol ls' >> loop opts file (i + 1)
                else
                  failWith
                    "the build still fails, but no further dependency can be added automatically (see output above)"

data Outcome = Did String String | Note String

render :: Outcome -> String
render (Did p lbl) = "  + added " ++ p ++ " to " ++ lbl
render (Note s) = "  ! " ++ s

reportUnresolved :: (Maybe Component, String) -> IO ()
reportUnresolved (_, m) =
  putStrLn
    ( "  ? cannot tell which package provides module " ++ m
        ++ ". If it is one of your own modules, list it in other-modules/exposed-modules;"
        ++ " otherwise add the package by hand."
    )

-- | Apply all findings to the cabal file lines.
applyAll :: String -> [String] -> [Finding] -> ([String], [Outcome])
applyAll pname ls0 = foldl' go (ls0, [])
  where
    go (ls, outs) f = case target f of
      Left why -> (ls, outs ++ [Note why])
      Right mc ->
        let p = fPackage f
            lbl = maybe "the main library" describeComponent mc
         in if maybe False (`selfDep` p) mc
              then
                ( ls
                , outs
                    ++ [ Note
                           ( "module needed by " ++ lbl ++ " lives in " ++ p ++ ", i.e. the same component: "
                               ++ "list it in other-modules/exposed-modules (or fix hs-source-dirs), not build-depends"
                           )
                       ]
                )
              else case addDependency mc p ls of
              Added ls' -> (ls', outs ++ [Did p lbl])
              AlreadyPresent ->
                ( ls
                , outs
                    ++ [ Note
                           ( "GHC still reports " ++ p ++ " as missing for " ++ lbl
                               ++ ", but it is already in build-depends (check bounds, flags, conditionals)"
                           )
                       ]
                )
              NoSuchStanza d -> (ls, outs ++ [Note ("no stanza found for " ++ d ++ ", could not add " ++ p)])

    -- does the dependency name the component itself?
    selfDep c p = compKind c == KLibrary && case break (== ':') p of
      (pk, rest)
        | map toLower pk == map toLower pname -> case rest of
            [] -> compName c == Nothing
            (_ : l) -> compName c == Just l
      _ -> False

    target f = case fComponent f of
      Nothing -> Right Nothing
      Just (c, p)
        | map toLower p /= map toLower pname ->
            Left ("skipping " ++ fPackage f ++ ": it belongs to package " ++ p ++ ", not " ++ pname)
        | otherwise -> Right (Just c)

------------------------------------------------------------------------------
-- IO helpers

failWith :: String -> IO a
failWith m = hPutStrLn stderr ("error: " ++ m) >> exitWith (ExitFailure 1)

locateCabalFile :: IO FilePath
locateCabalFile = do
  fs <- listDirectory "."
  case sort [f | f <- fs, takeExtension f == ".cabal", take 1 f /= "."] of
    [f] -> return f
    [] -> failWith "no .cabal file in the current directory (use --cabal-file)"
    many -> failWith ("several .cabal files found: " ++ unwords many ++ " (use --cabal-file)")

-- | Run cabal with stdout and stderr merged, echoing every line as it arrives.
runCabal :: [String] -> IO (ExitCode, [String])
runCabal args = do
  (r, w) <- createPipe
  enc <- mkTextEncoding "UTF-8//IGNORE"
  hSetEncoding r enc
  started <- try (createProcess (proc "cabal" args) {std_out = UseHandle w, std_err = UseHandle w})
  case started of
    Left e -> failWith ("could not start cabal: " ++ show (e :: IOException))
    Right (_, _, _, ph) -> do
      ls <- readLines r []
      hClose r
      code <- waitForProcess ph
      return (code, ls)
  where
    readLines h acc = do
      eof <- hIsEOF h
      if eof
        then return (reverse acc)
        else do
          l <- hGetLine h
          putStrLn l
          hFlush stdout
          readLines h (l : acc)

-- | Read the cabal file as lines, remembering its line ending.
readCabal :: FilePath -> IO ([String], String)
readCabal f = do
  s <- withFile f ReadMode $ \h -> do
    hSetEncoding h utf8
    hSetNewlineMode h noNewlineTranslation
    c <- hGetContents h
    _ <- evaluate (length c)
    return c
  let eol = if "\r\n" `isInfixOf` s then "\r\n" else "\n"
  return (map (filter (/= '\r')) (lines s), eol)

writeCabal :: FilePath -> String -> [String] -> IO ()
writeCabal f eol ls = withFile f WriteMode $ \h -> do
  hSetEncoding h utf8
  hSetNewlineMode h noNewlineTranslation
  hPutStr h (concatMap (++ eol) ls)
