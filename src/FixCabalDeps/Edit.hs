-- | Text-level editing of a @.cabal@ file. Works on lines so that comments,
-- formatting and comma style of the existing file are preserved.
module FixCabalDeps.Edit
  ( EditResult (..)
  , addDependency
  , wouldCreateCycle
  , packageName
  ) where

import Data.Char (isAlphaNum, isSpace, toLower)
import Data.List (dropWhileEnd, find, findIndex, isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe, listToMaybe)

import FixCabalDeps.Types

data EditResult
  = Added [String]
  | AlreadyPresent
  | NoSuchStanza String
  deriving (Eq, Show)

data Stanza = Stanza
  { zComp :: Component
  , _zStart :: Int -- ^ header line
  , _zEnd :: Int -- ^ first line after the stanza
  }

-- | The @name:@ field of the package.
packageName :: [String] -> Maybe String
packageName ls =
  listToMaybe
    [ trim (drop 5 l)
    | l <- ls
    , not (null l)
    , not (isSpace (head l))
    , "name:" `isPrefixOf` map toLower l
    ]

-- | Add @pkg@ to @build-depends@ of the given component ('Nothing' = the main
-- library, or the first stanza if there is none).
addDependency :: Maybe Component -> String -> [String] -> EditResult
addDependency mcomp pkg ls =
  case chooseStanza of
    Nothing -> NoSuchStanza (maybe "library" describeComponent mcomp)
    Just st -> editStanza st
  where
    stanzas = findStanzas ls

    chooseStanza = case mcomp of
      Just c -> find ((== c) . zComp) stanzas
      Nothing -> case filter ((== KLibrary) . compKind . zComp) stanzas of
        (s : _) -> Just s
        [] -> listToMaybe stanzas

    editStanza (Stanza _ s e) =
      case field of
        Nothing -> Added (insertAt insertPoint newField ls)
        Just i -> editField i
      where
        idxs = [s + 1 .. e - 1]
        content = filter (isContent . (ls !!)) idxs
        base = if null content then 2 else minimum (map (indentOf . (ls !!)) content)
        field =
          find
            ( \i ->
                indentOf (ls !! i) == base
                  && "build-depends:" `isPrefixOf` map toLower (trim (ls !! i))
            )
            content
        -- keep `import:` lines at the top of the stanza
        insertPoint = s + 1 + length (takeWhile isImport (map (ls !!) idxs))

        editField i
          | map toLower pkg `elem` map (map toLower) (depNames fieldLines) = AlreadyPresent
          | k == i = Added (singleLine i)
          | leading = Added (insertAt (k + 1) (ind ++ ", " ++ pkg) ls)
          | otherwise = Added (insertAt (k + 1) (ind ++ pkg) (replaceAt k (addComma (ls !! k)) ls))
          where
            range =
              takeWhile
                (\j -> let l = ls !! j in not (isContent l) || indentOf l > base)
                [i + 1 .. e - 1]
            contIdx = i : filter (isContent . (ls !!)) range
            k = last contIdx
            fieldLines = map (ls !!) contIdx
            ind = takeWhile isSpace (ls !! k)
            leading = any (\j -> "," `isPrefixOf` trim (ls !! j)) (tail contIdx)

        -- a new field, with its value in the same column as its sibling fields
        newField =
          let siblingCol =
                listToMaybe
                  [c | j <- content, indentOf (ls !! j) == base, Just c <- [valueColOf (ls !! j)]]
              col = max (base + 15) (fromMaybe 0 siblingCol)
           in replicate base ' ' ++ "build-depends:" ++ replicate (col - base - 14) ' ' ++ pkg

        -- `build-depends: a`  ->  one item per line, new item under the first one
        singleLine i = case valueColOf l of
          Nothing -> replaceAt i (l ++ " " ++ pkg) ls
          Just c -> insertAt (i + 1) (replicate c ' ' ++ pkg) (replaceAt i (addComma l) ls)
          where
            l = dropWhileEnd isSpace (ls !! i)

-- | Would adding @pkg@ to the library @cur@ close a dependency cycle between
-- libraries of the same package (@pname@)?
wouldCreateCycle :: String -> Component -> String -> [String] -> Bool
wouldCreateCycle pname cur pkg ls
  | compKind cur /= KLibrary = False
  | otherwise = maybe False (reaches []) (resolve (map toLower pkg))
  where
    lname = map toLower pname
    stanzas = findStanzas ls
    libs = [zComp s | s <- stanzas, compKind (zComp s) == KLibrary]

    findLib l = find (\c -> fmap (map toLower) (compName c) == Just l) libs
    resolve n
      | n == lname = find ((== Nothing) . compName) libs
      | (pk, ':' : l) <- break (== ':') n, pk == lname = findLib l
      | otherwise = Nothing
    resolveDep n = maybe (findLib n) Just (resolve n)

    depsOf c = [t | s <- stanzas, zComp s == c, n <- stanzaDeps ls s, Just t <- [resolveDep (map toLower n)]]

    -- is `cur` reachable from c?
    reaches seen c
      | c == cur = True
      | c `elem` seen = False
      | otherwise = any (reaches (c : seen)) (depsOf c)

-- | Every name listed in any @build-depends@ field of the stanza.
stanzaDeps :: [String] -> Stanza -> [String]
stanzaDeps ls (Stanza _ s e) = go (s + 1)
  where
    go j
      | j >= e = []
      | isContent l && "build-depends:" `isPrefixOf` map toLower (trim l) =
          let ind = indentOf l
              cont = takeWhile (\k -> let m = ls !! k in not (isContent m) || indentOf m > ind) [j + 1 .. e - 1]
              fieldLines = l : [ls !! k | k <- cont, isContent (ls !! k)]
           in depNames fieldLines ++ go (j + 1 + length cont)
      | otherwise = go (j + 1)
      where
        l = ls !! j

------------------------------------------------------------------------------
-- Stanza discovery

findStanzas :: [String] -> [Stanza]
findStanzas ls = [Stanza c i (endOf i) | (i, l) <- zip [0 ..] ls, Just c <- [parseHeader l]]
  where
    endOf i = case findIndex isTopLevel (drop (i + 1) ls) of
      Just k -> i + 1 + k
      Nothing -> length ls

parseHeader :: String -> Maybe Component
parseHeader l
  | null l || isSpace (head l) = Nothing
  | otherwise = case words l of
      (w : rest) -> case map toLower w of
        "library" -> Just (Component KLibrary (nameOf rest))
        "executable" -> Just (Component KExecutable (nameOf rest))
        "test-suite" -> Just (Component KTestSuite (nameOf rest))
        "benchmark" -> Just (Component KBenchmark (nameOf rest))
        _ -> Nothing
      [] -> Nothing
  where
    nameOf [] = Nothing
    nameOf ws = Just (unwords ws)

------------------------------------------------------------------------------
-- Helpers

-- | Names (without version bounds) in the lines of a @build-depends@ field.
depNames :: [String] -> [String]
depNames [] = []
depNames (first : more) =
  [ takeWhile isNameChar t
  | item <- concatMap splitComma texts
  , let t = trim item
  , not (null t)
  ]
  where
    texts = drop (length "build-depends:") (trim first) : map trim more
    isNameChar c = isAlphaNum c || c == '-' || c == '_' || c == ':'

splitComma :: String -> [String]
splitComma s = case break (== ',') s of
  (a, []) -> [a]
  (a, _ : r) -> a : splitComma r

isImport :: String -> Bool
isImport l = "import:" `isPrefixOf` map toLower (trim l)

-- | Column where the value of a @key:   value@ line starts.
valueColOf :: String -> Maybe Int
valueColOf l = case break (== ':') (dropWhile isSpace l) of
  (key, ':' : v)
    | not (null (trim v)) -> Just (indentOf l + length key + 1 + length (takeWhile isSpace v))
  _ -> Nothing

addComma :: String -> String
addComma l = let r = dropWhileEnd isSpace l in if "," `isSuffixOf` r then r else r ++ ","

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

indentOf :: String -> Int
indentOf = length . takeWhile isSpace

isComment :: String -> Bool
isComment l = "--" `isPrefixOf` trim l

isContent :: String -> Bool
isContent l = not (null (trim l)) && not (isComment l)

isTopLevel :: String -> Bool
isTopLevel l = isContent l && not (isSpace (head l))

insertAt :: Int -> a -> [a] -> [a]
insertAt n x xs = let (a, b) = splitAt n xs in a ++ x : b

replaceAt :: Int -> a -> [a] -> [a]
replaceAt n x xs = let (a, b) = splitAt n xs in a ++ x : drop 1 b
