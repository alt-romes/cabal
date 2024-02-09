{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.Glob
-- Copyright   :  Isaac Jones, Simon Marlow 2003-2004
-- License     :  BSD3
--                portions Copyright (c) 2007, Galois Inc.
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Simple file globbing.
module Distribution.Simple.Glob where

import Distribution.Compat.Prelude
import Prelude ()

import Control.Monad (mapM)

import Distribution.Parsec
import Distribution.Pretty

import Distribution.CabalSpecVersion
import Distribution.Simple.Utils
import Distribution.Verbosity hiding (normal)

import Data.List (stripPrefix)
import System.Directory
import System.FilePath

import Distribution.Simple.Errors

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

--------------------------------------------------------------------------------

-- | A file path specified by globbing
data FilePathGlob = FilePathGlob FilePathRoot FilePathGlobRel
  deriving (Eq, Show, Generic)

data FilePathGlobRel
  = GlobDir !Glob !FilePathGlobRel
  | -- | A glob ending in @**/<glob>@, where @**@ denotes recursively traversing
    -- all directories and matching filenames on <glob>.
    GlobDirRecursive !Glob
  | GlobFile !Glob
  | -- | trailing dir, a glob ending in @/@
    GlobDirTrailing
  deriving (Eq, Show, Generic)

-- | A single directory or file component of a globbed path
type Glob = [GlobPiece]

-- | A piece of a globbing pattern
data GlobPiece
  = -- | A wildcard @*@
    WildCard
  | -- | A literal string @dirABC@
    Literal String
  | -- | A union of patterns, e.g. @dir/{a,*.txt,c}/...@
    Union [Glob]
  deriving (Eq, Show, Generic)

data FilePathRoot
  = FilePathRelative
  | -- | e.g. @"/"@, @"c:\"@ or result of 'takeDrive'
    FilePathRoot FilePath
  | FilePathHomeDir
  deriving (Eq, Show, Generic)

instance Binary FilePathGlob
instance Binary FilePathRoot
instance Binary FilePathGlobRel
instance Binary GlobPiece

instance Structured FilePathGlob
instance Structured FilePathRoot
instance Structured FilePathGlobRel
instance Structured GlobPiece

-- | Check if a 'FilePathGlob' doesn't actually make use of any globbing and
-- is in fact equivalent to a non-glob 'FilePath'.
--
-- If it is trivial in this sense then the result is the equivalent constant
-- 'FilePath'. On the other hand if it is not trivial (so could in principle
-- match more than one file) then the result is @Nothing@.
isTrivialFilePathGlob :: FilePathGlob -> Maybe FilePath
isTrivialFilePathGlob (FilePathGlob root pathglob) =
  case root of
    FilePathRelative -> go [] pathglob
    FilePathRoot root' -> go [root'] pathglob
    FilePathHomeDir -> Nothing
  where
    go paths (GlobDir [Literal path] globs) = go (path : paths) globs
    go paths (GlobFile [Literal path]) = Just (joinPath (reverse (path : paths)))
    go paths GlobDirTrailing =
      Just
        ( addTrailingPathSeparator
            (joinPath (reverse paths))
        )
    go _ _ = Nothing

-- | Get the 'FilePath' corresponding to a 'FilePathRoot'.
--
-- The 'FilePath' argument is required to supply the path for the
-- 'FilePathRelative' case.
getFilePathRootDirectory
  :: FilePathRoot
  -> FilePath
  -- ^ root for relative paths
  -> IO FilePath
getFilePathRootDirectory FilePathRelative root = return root
getFilePathRootDirectory (FilePathRoot root) _ = return root
getFilePathRootDirectory FilePathHomeDir _ = getHomeDirectory

-------------------------------------------------------------------------------

-- * Matching

--------------------------------------------------------------------------------

-- | Match a 'FilePathGlob' against the file system, starting from a given
-- root directory for relative paths. The results of relative globs are
-- relative to the given root. Matches for absolute globs are absolute.
matchFileGlob :: FilePath -> FilePathGlob -> IO [FilePath]
matchFileGlob relroot (FilePathGlob globroot glob) = do
  root <- getFilePathRootDirectory globroot relroot
  matches <- matchFileGlobRel root glob
  case globroot of
    FilePathRelative -> return matches
    _ -> return (map (root </>) matches)

-- | Match a 'FilePathGlobRel' against the file system, starting from a
-- given root directory. The results are all relative to the given root.
matchFileGlobRel :: FilePath -> FilePathGlobRel -> IO [FilePath]
matchFileGlobRel root glob =
  -- For this function, which is the general globbing one (doesn't care about
  -- cabal spec, used e.g. for monitoring), we consider all matches.
  mapMaybe
    ( \case
        GlobMatch a -> Just a
        GlobWarnMultiDot a -> Just a
        GlobMatchesDirectory a -> Just a
        GlobMissingDirectory{} -> Nothing
    )
    <$> runDirFileGlob silent Nothing root glob

-- | Match a globbing pattern against a file path component
matchGlob :: Glob -> String -> Bool
matchGlob = goStart
  where
    -- From the man page, glob(7):
    --   "If a filename starts with a '.', this character must be
    --    matched explicitly."

    go, goStart :: [GlobPiece] -> String -> Bool

    goStart (WildCard : _) ('.' : _) = False
    goStart (Union globs : rest) cs =
      any
        (\glob -> goStart (glob ++ rest) cs)
        globs
    goStart rest cs = go rest cs

    go [] "" = True
    go (Literal lit : rest) cs
      | Just cs' <- stripPrefix lit cs =
          go rest cs'
      | otherwise = False
    go [WildCard] "" = True
    go (WildCard : rest) (c : cs) = go rest (c : cs) || go (WildCard : rest) cs
    go (Union globs : rest) cs = any (\glob -> go (glob ++ rest) cs) globs
    go [] (_ : _) = False
    go (_ : _) "" = False

-------------------------------------------------------------------------------

-- * Parsing & printing

-- syntax (version) ->                    -> interpreter (version)
-- syntax           -> semantic (version) -> interpreter

--------------------------------------------------------------------------------
-- Filepaths with globs may be parsed in the special context is globbing in
-- cabal package fields, such as `data-files`. In that case, we restrict the
-- globbing syntax to that supported by the cabal spec version in use.
-- Otherwise, we parse the globs to the extent of our globbing features
-- (wildcards `*`, unions `{a,b,c}`, and directory-recursive wildcards `**`).

-- ** Parsing globs in a cabal package

parseFileGlob :: CabalSpecVersion -> FilePath -> Either GlobSyntaxError FilePathGlobRel
parseFileGlob version filepath = case reverse (splitDirectories filepath) of
  [] ->
    Left EmptyGlob
  (filename : "**" : segments)
    | allowGlobStar -> do
        finalSegment <- case splitExtensions filename of
          ("*", ext)
            | '*' `elem` ext -> Left StarInExtension
            | null ext -> Left NoExtensionOnStar
            | otherwise -> Right (GlobDirRecursive [WildCard, Literal ext])
          _
            | allowLiteralFilenameGlobStar ->
                Right (GlobDirRecursive [Literal filename])
            | otherwise ->
                Left LiteralFileNameGlobStar

        foldM addStem finalSegment segments
    | otherwise -> Left VersionDoesNotSupportGlobStar
  (filename : segments) -> do
    pat <- case splitExtensions filename of
      ("*", ext)
        | not allowGlob -> Left VersionDoesNotSupportGlob
        | '*' `elem` ext -> Left StarInExtension
        | null ext -> Left NoExtensionOnStar
        | otherwise -> Right (GlobFile [WildCard, Literal ext])
      (_, ext)
        | '*' `elem` ext -> Left StarInExtension
        | '*' `elem` filename -> Left StarInFileName
        | otherwise -> Right (GlobFile [Literal filename])

    foldM addStem pat segments
  where
    addStem pat seg
      | '*' `elem` seg = Left StarInDirectory
      | otherwise = Right (GlobDir [Literal seg] pat)
    allowGlob = version >= CabalSpecV1_6
    allowGlobStar = version >= CabalSpecV2_4
    allowLiteralFilenameGlobStar = version >= CabalSpecV3_8

enableMultidot :: CabalSpecVersion -> Bool
enableMultidot version
  | version >= CabalSpecV2_4 = True
  | otherwise = False

-- ** Parsing globs otherwise

instance Pretty FilePathGlob where
  pretty (FilePathGlob root pathglob) = pretty root Disp.<> pretty pathglob

instance Parsec FilePathGlob where
  parsec = do
    root <- parsec
    case root of
      FilePathRelative -> FilePathGlob root <$> parsec
      _ -> FilePathGlob root <$> parsec <|> pure (FilePathGlob root GlobDirTrailing)

instance Pretty FilePathRoot where
  pretty FilePathRelative = Disp.empty
  pretty (FilePathRoot root) = Disp.text root
  pretty FilePathHomeDir = Disp.char '~' Disp.<> Disp.char '/'

instance Parsec FilePathRoot where
  parsec = root <|> P.try home <|> P.try drive <|> pure FilePathRelative
    where
      root = FilePathRoot "/" <$ P.char '/'
      home = FilePathHomeDir <$ P.string "~/"
      drive = do
        dr <- P.satisfy $ \c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
        _ <- P.char ':'
        _ <- P.char '/' <|> P.char '\\'
        return (FilePathRoot (toUpper dr : ":\\"))

instance Pretty FilePathGlobRel where
  pretty (GlobDir glob pathglob) =
    dispGlob glob
      Disp.<> Disp.char '/'
      Disp.<> pretty pathglob
  pretty (GlobDirRecursive glob) =
    Disp.text "**/"
      Disp.<> dispGlob glob
  pretty (GlobFile glob) = dispGlob glob
  pretty GlobDirTrailing = Disp.empty

instance Parsec FilePathGlobRel where
  parsec = parsecPath
    where
      parsecPath :: CabalParsing m => m FilePathGlobRel
      parsecPath = do
        glob <- parsecGlob
        dirSep *> (GlobDir glob <$> parsecPath <|> pure (GlobDir glob GlobDirTrailing)) <|> pure (GlobFile glob)
      -- We could support parsing recursive directory search syntax
      -- @**@ here too, rather than just in 'parseFileGlob'

      dirSep :: CabalParsing m => m ()
      dirSep =
        () <$ P.char '/'
          <|> P.try
            ( do
                _ <- P.char '\\'
                -- check this isn't an escape code
                P.notFollowedBy (P.satisfy isGlobEscapedChar)
            )

      parsecGlob :: CabalParsing m => m Glob
      parsecGlob = some parsecPiece
        where
          parsecPiece = P.choice [literal, wildcard, union]

          wildcard = WildCard <$ P.char '*'
          union = Union . toList <$> P.between (P.char '{') (P.char '}') (P.sepByNonEmpty parsecGlob (P.char ','))
          literal = Literal <$> some litchar

          litchar = normal <|> escape

          normal = P.satisfy (\c -> not (isGlobEscapedChar c) && c /= '/' && c /= '\\')
          escape = P.try $ P.char '\\' >> P.satisfy isGlobEscapedChar

--------------------------------------------------------------------------------
-- Parse and printing utils
--------------------------------------------------------------------------------

dispGlob :: Glob -> Disp.Doc
dispGlob = Disp.hcat . map dispPiece
  where
    dispPiece WildCard = Disp.char '*'
    dispPiece (Literal str) = Disp.text (escape str)
    dispPiece (Union globs) =
      Disp.braces
        ( Disp.hcat
            ( Disp.punctuate
                (Disp.char ',')
                (map dispGlob globs)
            )
        )
    escape [] = []
    escape (c : cs)
      | isGlobEscapedChar c = '\\' : c : escape cs
      | otherwise = c : escape cs

isGlobEscapedChar :: Char -> Bool
isGlobEscapedChar '*' = True
isGlobEscapedChar '{' = True
isGlobEscapedChar '}' = True
isGlobEscapedChar ',' = True
isGlobEscapedChar _ = False

-- ** Cabal package globbing errors

data GlobSyntaxError
  = StarInDirectory
  | StarInFileName
  | StarInExtension
  | NoExtensionOnStar
  | EmptyGlob
  | LiteralFileNameGlobStar
  | VersionDoesNotSupportGlobStar
  | VersionDoesNotSupportGlob
  deriving (Eq, Show)

explainGlobSyntaxError :: FilePath -> GlobSyntaxError -> String
explainGlobSyntaxError filepath StarInDirectory =
  "invalid file glob '"
    ++ filepath
    ++ "'. A wildcard '**' is only allowed as the final parent"
    ++ " directory. Stars must not otherwise appear in the parent"
    ++ " directories."
explainGlobSyntaxError filepath StarInExtension =
  "invalid file glob '"
    ++ filepath
    ++ "'. Wildcards '*' are only allowed as the"
    ++ " file's base name, not in the file extension."
explainGlobSyntaxError filepath StarInFileName =
  "invalid file glob '"
    ++ filepath
    ++ "'. Wildcards '*' may only totally replace the"
    ++ " file's base name, not only parts of it."
explainGlobSyntaxError filepath NoExtensionOnStar =
  "invalid file glob '"
    ++ filepath
    ++ "'. If a wildcard '*' is used it must be with an file extension."
explainGlobSyntaxError filepath LiteralFileNameGlobStar =
  "invalid file glob '"
    ++ filepath
    ++ "'. Prior to 'cabal-version: 3.8'"
    ++ " if a wildcard '**' is used as a parent directory, the"
    ++ " file's base name must be a wildcard '*'."
explainGlobSyntaxError _ EmptyGlob =
  "invalid file glob. A glob cannot be the empty string."
explainGlobSyntaxError filepath VersionDoesNotSupportGlobStar =
  "invalid file glob '"
    ++ filepath
    ++ "'. Using the double-star syntax requires 'cabal-version: 2.4'"
    ++ " or greater. Alternatively, for compatibility with earlier Cabal"
    ++ " versions, list the included directories explicitly."
explainGlobSyntaxError filepath VersionDoesNotSupportGlob =
  "invalid file glob '"
    ++ filepath
    ++ "'. Using star wildcards requires 'cabal-version: >= 1.6'. "
    ++ "Alternatively if you require compatibility with earlier Cabal "
    ++ "versions then list all the files explicitly."

-- Note throughout that we use splitDirectories, not splitPath. On
-- Posix, this makes no difference, but, because Windows accepts both
-- slash and backslash as its path separators, if we left in the
-- separators from the glob we might not end up properly normalised.

data GlobResult a
  = -- | The glob matched the value supplied.
    GlobMatch a
  | -- | The glob did not match the value supplied because the
    --   cabal-version is too low and the extensions on the file did
    --   not precisely match the glob's extensions, but rather the
    --   glob was a proper suffix of the file's extensions; i.e., if
    --   not for the low cabal-version, it would have matched.
    GlobWarnMultiDot a
  | -- | The glob couldn't match because the directory named doesn't
    --   exist. The directory will be as it appears in the glob (i.e.,
    --   relative to the directory passed to 'matchDirFileGlob', and,
    --   for 'data-files', relative to 'data-dir').
    GlobMissingDirectory a
  | -- | The glob matched a directory when we were looking for files only. It didn't match a file!
    GlobMatchesDirectory a
  deriving (Show, Eq, Ord, Functor)

-- | Extract the matches from a list of 'GlobResult's.
--
-- Note: throws away the 'GlobMissingDirectory' results; chances are
-- that you want to check for these and error out if any are present.
globMatches :: [GlobResult a] -> [a]
globMatches input = [a | GlobMatch a <- input]

-- | This will 'die'' when the glob matches no files, or if the glob
-- refers to a missing directory, or if the glob fails to parse.
--
-- The 'Version' argument must be the spec version of the package
-- description being processed, as globs behave slightly differently
-- in different spec versions.
--
-- The first 'FilePath' argument is the directory that the glob is
-- relative to. It must be a valid directory (and hence it can't be
-- the empty string). The returned values will not include this
-- prefix.
--
-- The second 'FilePath' is the glob itself.
matchDirFileGlob :: Verbosity -> CabalSpecVersion -> FilePath -> FilePath -> IO [FilePath]
matchDirFileGlob v = matchDirFileGlobWithDie v dieWithException

-- | Like 'matchDirFileGlob' but with customizable 'die'
--
-- @since 3.6.0.0
matchDirFileGlobWithDie :: Verbosity -> (Verbosity -> CabalException -> IO [FilePath]) -> CabalSpecVersion -> FilePath -> FilePath -> IO [FilePath]
matchDirFileGlobWithDie verbosity rip version dir filepath = case parseFileGlob version filepath of
  Left err -> rip verbosity $ MatchDirFileGlob (explainGlobSyntaxError filepath err)
  Right glob -> do
    results <- runDirFileGlob verbosity (Just version) dir glob
    let missingDirectories =
          [missingDir | GlobMissingDirectory missingDir <- results]
        matches = globMatches results
        directoryMatches = [a | GlobMatchesDirectory a <- results]

    let errors :: [String]
        errors =
          [ "filepath wildcard '"
            ++ filepath
            ++ "' refers to the directory"
            ++ " '"
            ++ missingDir
            ++ "', which does not exist or is not a directory."
          | missingDir <- missingDirectories
          ]
            ++ [ "filepath wildcard '" ++ filepath ++ "' does not match any files."
               | null matches && null directoryMatches
               -- we don't error out on directory matches, simply warn about them and ignore.
               ]

        warns :: [String]
        warns =
          [ "Ignoring directory '" ++ path ++ "'" ++ " listed in a Cabal package field which should only include files (not directories)."
          | path <- directoryMatches
          ]

    if null errors
      then do
        unless (null warns) $
          warn verbosity $
            unlines warns
        return matches
      else rip verbosity $ MatchDirFileGlobErrors errors

-- | Match files against a pre-parsed glob, starting in a directory.
--
-- The 'Version' argument must be the spec version of the package
-- description being processed, as globs behave slightly differently
-- in different spec versions.
--
-- The 'FilePath' argument is the directory that the glob is relative
-- to. It must be a valid directory (and hence it can't be the empty
-- string). The returned values will not include this prefix.
runDirFileGlob
  :: Verbosity
  -> Maybe CabalSpecVersion
  -- ^ If the glob we are running should care about the cabal spec, and warnings such as 'GlobWarnMultiDot', then this should be the version.
  -- If you want to run a glob but don't care about any of the cabal-spec restrictions on globs, use 'Nothing'!
  -> FilePath
  -> FilePathGlobRel
  -> IO [GlobResult FilePath]
runDirFileGlob verbosity mspec rawRoot pat = do
  -- The default data-dir is null. Our callers -should- be
  -- converting that to '.' themselves, but it's a certainty that
  -- some future call-site will forget and trigger a really
  -- hard-to-debug failure if we don't check for that here.
  when (null rawRoot) $
    warn verbosity $
      "Null dir passed to runDirFileGlob; interpreting it "
        ++ "as '.'. This is probably an internal error."
  let root = if null rawRoot then "." else rawRoot
  debug verbosity $ "Expanding glob '" ++ show (pretty pat) ++ "' in directory '" ++ root ++ "'."
  -- This function might be called from the project root with dir as
  -- ".". Walking the tree starting there involves going into .git/
  -- and dist-newstyle/, which is a lot of work for no reward, so
  -- extract the constant prefix from the pattern and start walking
  -- there, and only walk as much as we need to: recursively if **,
  -- the whole directory if *, and just the specific file if it's a
  -- literal.
  let
    (prefixSegments, variablePattern) = splitConstantPrefix pat
    joinedPrefix = joinPath prefixSegments

    -- The glob matching function depends on whether we care about the cabal version or not
    doesGlobMatch :: Glob -> FilePath -> String -> IO (Maybe (GlobResult FilePath))
    doesGlobMatch glob dir str = do
      case mspec of
        Nothing -> do
          return $ if matchGlob glob str then Just (GlobMatch (dir </> str)) else Nothing
        Just spec -> do
          let match = fmap (dir </> str <$) $ checkNameMatches spec glob str

          -- precondition: the filepath exists
          isFile <- doesFileExist (root </> dir </> str)

          -- When running a glob from a Cabal package description (i.e.
          -- when a cabal spec version is passed as an argument), we
          -- disallow matching a @GlobFile@ against a directory, preferring
          -- @GlobDir dir GlobDirTrailing@ to specify a directory match.
          let adapt (GlobMatch x) = GlobMatchesDirectory x
              adapt (GlobWarnMultiDot x) = GlobMatchesDirectory x
              adapt (GlobMatchesDirectory x) = GlobMatchesDirectory x
              -- this should never match, unless you are in a file-delete-heavy concurrent setting i guess
              adapt (GlobMissingDirectory x) = GlobMissingDirectory x

          return $ if isFile then match else fmap adapt match

    go :: FilePathGlobRel -> FilePath -> IO [GlobResult FilePath]
    go (GlobFile glob) dir = do
      entries <- getDirectoryContents (root </> dir)
      catMaybes <$> mapM (doesGlobMatch glob dir) entries
    go (GlobDirRecursive glob) dir = do
      entries <- getDirectoryContentsRecursive (root </> dir)
      catMaybes <$> for entries (doesGlobMatch glob dir . takeFileName)
    go (GlobDir glob globPath) dir = do
      entries <- getDirectoryContents (root </> dir)
      subdirs <-
        filterM
          ( \subdir ->
              doesDirectoryExist
                (root </> dir </> subdir)
          )
          $ filter (matchGlob glob) entries
      concat <$> traverse (\subdir -> go globPath (dir </> subdir)) subdirs
    go GlobDirTrailing dir = return [GlobMatch dir]

  directoryExists <- doesDirectoryExist (root </> joinedPrefix)
  if directoryExists
    then go variablePattern joinedPrefix
    else return [GlobMissingDirectory joinedPrefix]
  where
    -- \| Extract the (possibly null) constant prefix from the pattern.
    -- This has the property that, if @(pref, final) = splitConstantPrefix pat@,
    -- then @pat === foldr GlobDir final pref@.
    splitConstantPrefix :: FilePathGlobRel -> ([FilePath], FilePathGlobRel)
    splitConstantPrefix = unfoldr' step
      where
        step (GlobDir [Literal seg] pat') = Right (seg, pat')
        step pat' = Left pat'

        unfoldr' :: (a -> Either r (b, a)) -> a -> ([b], r)
        unfoldr' f a = case f a of
          Left r -> ([], r)
          Right (b, a') -> case unfoldr' f a' of
            (bs, r) -> (b : bs, r)

-- | Is the root of this relative glob path a directory-recursive wildcard, e.g. @**/*.txt@ ?
isRecursiveInRoot :: FilePathGlobRel -> Bool
isRecursiveInRoot (GlobDirRecursive _) = True
isRecursiveInRoot _ = False

-- | Check how the string matches the glob under this cabal version
checkNameMatches :: CabalSpecVersion -> Glob -> String -> Maybe (GlobResult FilePath)
checkNameMatches spec glob candidate
  -- Check if glob matches in its general form
  | enableMultidot spec && matchGlob glob candidate =
      -- if multidot is supported, then this is a clean match
      pure (GlobMatch candidate)
  | matchGlob glob candidate =
      -- if not, issue a warning saying multidot is needed for the match
      let (_, candidateExts) = splitExtensions $ takeFileName candidate
          extractExts :: Glob -> Maybe String
          extractExts [] = Nothing
          extractExts [Literal lit]
            -- Any literal terminating a glob, and which does have an extension,
            -- returns that extension. Otherwise, recurse until Nothing is returned.
            | let ext = takeExtensions lit
            , ext /= "" =
                Just ext
          extractExts (_ : x) = extractExts x
       in case extractExts glob of
            Just exts
              | exts == candidateExts ->
                  return (GlobMatch candidate)
              | exts `isSuffixOf` candidateExts ->
                  return (GlobWarnMultiDot candidate)
            _ -> return (GlobMatch candidate)
  | otherwise = empty

-- | How/does the glob match the given filepath, according to the cabal version?
-- Since this is pure, we don't make a distinction between matching on
-- directories or files (i.e. this function won't return 'GlobMatchesDirectory')
fileGlobMatches :: CabalSpecVersion -> FilePathGlobRel -> FilePath -> Maybe (GlobResult ())
fileGlobMatches version g path = go g (splitDirectories path)
  where
    go GlobDirTrailing [] =
      Just (GlobMatch ())
    go (GlobFile glob) [file] =
      checkNameMatches version glob file
    go (GlobDirRecursive glob) dirs
      | [] <- reverse dirs =
          Nothing -- @dir/**/x.txt@ should not match @dir/hello@
      | file : _ <- reverse dirs =
          checkNameMatches version glob file
    go (GlobDir glob globPath) (dir : dirs) = do
      _ <- checkNameMatches version glob dir -- we only care if dir segment matches
      go globPath dirs
    go _ _ = Nothing
