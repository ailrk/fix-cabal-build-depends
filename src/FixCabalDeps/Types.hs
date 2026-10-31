module FixCabalDeps.Types
  ( Kind (..)
  , Component (..)
  , Finding (..)
  , Analysis (..)
  , describeComponent
  ) where

data Kind = KLibrary | KExecutable | KTestSuite | KBenchmark
  deriving (Eq, Ord, Show)

-- | A component of a cabal package. 'Nothing' as name means the main library.
data Component = Component
  { compKind :: Kind
  , compName :: Maybe String
  }
  deriving (Eq, Ord, Show)

-- | A package that GHC (or our module table) says is missing.
data Finding = Finding
  { fComponent :: Maybe (Component, String)
    -- ^ component being built when the error appeared, plus its package name
  , fPackage :: String
  }
  deriving (Eq, Show)

data Analysis = Analysis
  { aFindings :: [Finding]
  , aUnresolved :: [(Maybe Component, String)]
    -- ^ modules that could not be found and could not be mapped to a package
  , aSolverFailure :: Bool
    -- ^ cabal's solver failed, so no build happened
  , aCabalFileError :: Bool
    -- ^ cabal could not parse the .cabal file
  }
  deriving (Eq, Show)

describeComponent :: Component -> String
describeComponent (Component k n) = kind ++ maybe "" (\x -> " '" ++ x ++ "'") n
  where
    kind = case k of
      KLibrary -> "library"
      KExecutable -> "executable"
      KTestSuite -> "test-suite"
      KBenchmark -> "benchmark"
