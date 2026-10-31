-- | Extract missing packages from the output of @cabal build@.
module FixCabalDeps.Parse
  ( analyse
  , parseComponent
  , stripVersion
  , normalisePackage
  ) where

import Control.Monad (msum)
import Data.Char (isDigit, isSpace)
import Data.List (dropWhileEnd, intercalate, isInfixOf, isPrefixOf, stripPrefix)

import FixCabalDeps.Types

data Pending = Pending
  { _pModule :: String
  , _pResolved :: Bool
  }

data St = St
  { stComp :: Maybe (Component, String)
  , stPending :: Maybe Pending
  , stFinds :: [Finding] -- reversed
  , stUnres :: [(Maybe Component, String)] -- reversed
  }

-- | Walk over the build output and collect what is missing, and where.
analyse :: [String] -> Analysis
analyse out =
  Analysis
    { aFindings = dedupe (reverse (stFinds final))
    , aUnresolved = dedupe (reverse (stUnres final))
    , aSolverFailure = any ("Could not resolve dependencies" `isInfixOf`) out
    , aCabalFileError = any ("Errors encountered when parsing cabal file" `isInfixOf`) out
    }
  where
    final = flush (foldl' step (St Nothing Nothing [] []) (map (dropWhileEnd (== '\r')) out))

step :: St -> String -> St
step st l
  | Just c <- parseComponent l = (flush st) {stComp = Just c}
  | Just m <- parseMissingModule l = (flush st) {stPending = Just (Pending m False)}
  | Just p <- hintPackage l =
      case stPending st of
        Just (Pending m False) -> emit p st {stPending = Just (Pending m True)}
        Just (Pending _ True) -> st
        Nothing -> emit p st
  | otherwise = st

-- | Finish the module we were looking at: if GHC gave no package hint for it,
-- report it as unresolved (e.g. one of the project's own modules).
flush :: St -> St
flush st = case stPending st of
  Just (Pending m False) -> st' {stUnres = (fmap fst (stComp st), m) : stUnres st'}
  _ -> st'
  where
    st' = st {stPending = Nothing}

emit :: String -> St -> St
emit p st = st {stFinds = Finding (stComp st) p : stFinds st}

dedupe :: Eq a => [a] -> [a]
dedupe = foldl' (\acc x -> if x `elem` acc then acc else acc ++ [x]) []

------------------------------------------------------------------------------
-- Line parsers

-- | @Building library for foo-0.1.0.0..@, @Building executable 'x' for ...@, etc.
parseComponent :: String -> Maybe (Component, String)
parseComponent l0 = do
  rest <- msum [stripPrefix "Building " l, stripPrefix "Preprocessing " l]
  (kind, r1) <- msum [(,) k <$> stripPrefix p rest | (p, k) <- kinds]
  let r2 = dropWhile isSpace r1
      (name, r3) = case r2 of
        ('\'' : xs) -> let (n, ys) = break (== '\'') xs in (Just n, drop 1 ys)
        _ -> (Nothing, r2)
  r4 <- stripPrefix "for " (dropWhile isSpace r3)
  let pid = takeWhile (not . isSpace) r4
  return (Component kind name, stripVersion (dropWhileEnd (== '.') pid))
  where
    l = dropWhile isSpace l0
    kinds =
      [ ("test suite", KTestSuite)
      , ("benchmark", KBenchmark)
      , ("executable", KExecutable)
      , ("library", KLibrary)
      ]

parseMissingModule :: String -> Maybe String
parseMissingModule l =
  msum
    [ after needle l >>= firstQuoted
    | needle <- ["Could not find module", "Could not load module", "Failed to load interface for"]
    ]

hintPackage :: String -> Maybe String
hintPackage l =
  normalisePackage
    <$> msum
      [ after "member of the hidden package" l >>= firstQuoted
      , after "you need to add" l >>= firstQuoted
      ]

-- | GHC's unit name -> what goes into build-depends:
--
-- > containers-0.6.7             -> containers
-- > filehub-0.1.3.0:filehub-core -> filehub:filehub-core   (internal library)
-- > foo-1.0:foo                  -> foo
normalisePackage :: String -> String
normalisePackage s = case break (== ':') s of
  (pk, []) -> stripVersion pk
  (pk, _ : lib) ->
    let p = stripVersion pk
     in if null lib || lib == p then p else p ++ ":" ++ lib

-- | Text following the first occurrence of the needle.
after :: String -> String -> Maybe String
after needle = go
  where
    go s
      | needle `isPrefixOf` s = Just (drop (length needle) s)
      | null s = Nothing
      | otherwise = go (tail s)

-- | First quoted token; GHC uses ‘…’ or '…' depending on the locale.
firstQuoted :: String -> Maybe String
firstQuoted s = case break (`elem` "‘'`") s of
  (_, _ : rest) -> Just (takeWhile (`notElem` "’'") rest)
  _ -> Nothing

-- | @containers-0.6.7@ -> @containers@
stripVersion :: String -> String
stripVersion s = case reverse (splitDash s) of
  (v : rest@(_ : _)) | isVersion v -> intercalate "-" (reverse rest)
  _ -> s
  where
    splitDash str = case break (== '-') str of
      (a, []) -> [a]
      (a, _ : r) -> a : splitDash r
    isVersion v = not (null v) && all (\c -> isDigit c || c == '.') v && isDigit (head v)
