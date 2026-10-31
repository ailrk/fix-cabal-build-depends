module Main (main) where

import Control.Monad (unless)
import Data.List (isPrefixOf)
import System.Exit (exitFailure)

import FixCabalDeps.Edit
import FixCabalDeps.Parse
import FixCabalDeps.Types

check :: (Eq a, Show a) => String -> a -> a -> IO Bool
check name expected actual
  | expected == actual = putStrLn ("ok   " ++ name) >> return True
  | otherwise = do
      putStrLn ("FAIL " ++ name)
      putStrLn ("  expected: " ++ show expected)
      putStrLn ("  actual:   " ++ show actual)
      return False

sampleOutput :: [String]
sampleOutput =
  [ "Resolving dependencies..."
  , "Building library for demo-0.1.0.0.."
  , "[1 of 1] Compiling Lib ( src/Lib.hs, dist/Lib.o )"
  , ""
  , "src/Lib.hs:3:1: error:"
  , "    Could not load module ‘Data.Map’."
  , "    It is a member of the hidden package ‘containers-0.6.7’."
  , "    Perhaps you need to add ‘containers’ to the build-depends in your .cabal file."
  , "Building executable 'demo' for demo-0.1.0.0.."
  , "app/Main.hs:5:1: error:"
  , "    Could not find module ‘Data.Aeson’"
  , "    Use -v (or `:set -v` in ghci) to see a list of the files searched for."
  , "Building test suite 'spec' for demo-0.1.0.0.."
  , "test/Spec.hs:2:1: error:"
  , "    Could not find module ‘My.Own.Module’"
  ]

lib, exe, spec :: Component
lib = Component KLibrary Nothing
exe = Component KExecutable (Just "demo")
spec = Component KTestSuite (Just "spec")

cabalTrailing :: [String]
cabalTrailing =
  [ "name:    demo"
  , ""
  , "library"
  , "  exposed-modules: Lib"
  , "  build-depends:"
  , "      base >=4 && <5,"
  , "      text"
  , "  hs-source-dirs: src"
  , ""
  , "executable demo"
  , "  main-is: Main.hs"
  , "  build-depends: base"
  ]

cabalLeading :: [String]
cabalLeading =
  [ "library"
  , "  build-depends:"
  , "      base >=4 && <5"
  , "    , text"
  , "  default-language: Haskell2010"
  ]

cabalNone :: [String]
cabalNone =
  [ "executable demo"
  , "  import: warnings"
  , "  main-is: Main.hs"
  ]

noResolve :: FilePath -> [Component]
noResolve _ = []

-- two components built in parallel: the "Building" lines say utils, the error is core's
interleaved :: [String]
interleaved =
  [ "Building library 'core' for foo-1.0.."
  , "Building library 'utils' for foo-1.0.."
  , "core/A.hs:1:1: error: [GHC-87110]"
  , "    Could not load module ‘B’."
  , "    It is a member of the hidden package ‘foo-1.0:utils’."
  , "utils/C.hs:7:1: error:"
  , "    Could not load module ‘Data.Map’."
  , "    It is a member of the hidden package ‘containers-0.6.7’."
  ]

byDir :: FilePath -> [Component]
byDir p
  | "core/" `isPrefixOf` p = [Component KLibrary (Just "core")]
  | "utils/" `isPrefixOf` p = [Component KLibrary (Just "utils")]
  | otherwise = []

cycleCabal :: [String]
cycleCabal =
  [ "name: foo", "library core", "  build-depends: base, foo:utils"
  , "library utils", "  build-depends: base" ]

chainCabal :: [String]
chainCabal =
  [ "name: foo"
  , "library a", "  build-depends: base"
  , "library b", "  build-depends: base, foo:a"
  , "library c", "  build-depends:", "      base,", "      b" ]

main :: IO ()
main = do
  let an = analyse "demo" noResolve sampleOutput
  rs <-
    sequence
      [ check
          "findings"
          [Finding (Just (lib, "demo")) "containers"]
          (aFindings an)
      , check
          "unresolved (no hint from GHC)"
          [(Just exe, "Data.Aeson"), (Just spec, "My.Own.Module")]
          (aUnresolved an)
      , check "solver ok" False (aSolverFailure an)
      , check
          "solver failure"
          True
          (aSolverFailure (analyse "demo" noResolve ["Error: [Cabal-7107] Could not resolve dependencies:"]))
      , check "ascii quotes" ["foo"]
          [ p | Finding _ p <- aFindings (analyse "demo" noResolve
              [ "    Could not find module 'Foo.Bar'"
              , "    It is a member of the hidden package 'foo-1.2.3'." ]) ]
      , check
          "internal library"
          ["filehub:filehub-core", "foo"]
          [ p | Finding _ p <- aFindings (analyse "demo" noResolve
              [ "    Could not load module ‘Filehub.Core’"
              , "    It is a member of the hidden package ‘filehub-0.1.3.0:filehub-core’."
              , "    Could not load module ‘Foo’"
              , "    It is a member of the hidden package ‘foo-1.0:foo’." ]) ]
      , check "cabal file error" True (aCabalFileError (analyse "demo" noResolve ["Errors encountered when parsing cabal file ./x.cabal:"]))
      , check "pkg:lib already present" AlreadyPresent
          (addDependency Nothing "filehub:filehub-core" ["library", "  build-depends: base, filehub:filehub-core"])
      , check "stripVersion" ("containers", "foo-bar", "base") (stripVersion "containers-0.6.7", stripVersion "foo-bar-1.2", stripVersion "base-4.18.0.0")
      , check "package name" (Just "demo") (packageName cabalTrailing)
      , -- editing
        check
          "trailing comma style"
          (Added
            [ "name:    demo", "", "library", "  exposed-modules: Lib", "  build-depends:"
            , "      base >=4 && <5,", "      text,", "      aeson"
            , "  hs-source-dirs: src", "", "executable demo", "  main-is: Main.hs", "  build-depends: base" ])
          (addDependency (Just lib) "aeson" cabalTrailing)
      , check
          "single-line field"
          (Added (take 11 cabalTrailing ++ ["  build-depends: base,", replicate 17 ' ' ++ "containers"]))
          (addDependency (Just exe) "containers" cabalTrailing)
      , check
          "leading comma style"
          (Added
            [ "library", "  build-depends:", "      base >=4 && <5", "    , text", "    , aeson"
            , "  default-language: Haskell2010" ])
          (addDependency Nothing "aeson" cabalLeading)
      , check "already present" AlreadyPresent (addDependency (Just lib) "TEXT" cabalTrailing)
      , check
          "no build-depends, after import"
          (Added ["executable demo", "  import: warnings", "  build-depends: base", "  main-is: Main.hs"])
          (addDependency (Just exe) "base" cabalNone)
      , check
          "new field aligned with siblings"
          (Added ["library", "  build-depends:" ++ replicate 10 ' ' ++ "base", "  hs-source-dirs:         src", "  exposed-modules:        Lib"])
          (addDependency Nothing "base" ["library", "  hs-source-dirs:         src", "  exposed-modules:        Lib"])
      , check
          "second dep goes under the first"
          (Added ["library", "  build-depends:" ++ replicate 10 ' ' ++ "base,", replicate 26 ' ' ++ "mtl"])
          (addDependency Nothing "mtl" ["library", "  build-depends:" ++ replicate 10 ' ' ++ "base"])
      , check
          "attribution by file path, not by last Building line"
          [ Finding (Just (Component KLibrary (Just "core"), "foo")) "foo:utils"
          , Finding (Just (Component KLibrary (Just "utils"), "foo")) "containers"
          ]
          (aFindings (analyse "foo" byDir interleaved))
      , check
          "source dirs"
          [(Component KLibrary (Just "core"), ["core", "gen"]), (Component KExecutable (Just "x"), ["."])]
          (sourceDirs ["library core", "  hs-source-dirs: core, gen", "executable x", "  main-is: M.hs"])
      , check "cycle detected" True (wouldCreateCycle "foo" (Component KLibrary (Just "utils")) "foo:core" cycleCabal)
      , check "no false cycle" False (wouldCreateCycle "foo" (Component KLibrary (Just "core")) "foo:utils" cycleCabal)
      , check "indirect cycle" True (wouldCreateCycle "foo" (Component KLibrary (Just "a")) "foo:c" chainCabal)
      , check "exe never cycles" False (wouldCreateCycle "foo" (Component KExecutable (Just "x")) "foo:core" cycleCabal)
      , check "unknown stanza" (NoSuchStanza "test-suite 'spec'") (addDependency (Just spec) "base" cabalTrailing)
      ]
  unless (and rs) exitFailure
