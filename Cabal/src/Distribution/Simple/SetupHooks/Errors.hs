{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

-----------------------------------------------------------------------------

-- Module      :  Distribution.Simple.SetupHooks.Errors
-- Copyright   :
-- License     :
--
-- Maintainer  :
-- Portability :
--
-- Exceptions for the Hooks build-type.

module Distribution.Simple.SetupHooks.Errors
  ( SetupHooksException (..)
  , CannotApplyComponentDiffReason (..)
  , IllegalComponentDiffReason (..)
  , RulesException (..)
  , InvalidRuleLocations (..)
  , InvalidRuleInputLocationReason (..)
  , InvalidRuleOutputLocationReason (..)
  , setupHooksExceptionCode
  , setupHooksExceptionMessage
  , showLocs
  ) where

import Distribution.PackageDescription
import Distribution.Simple.SetupHooks.Rule
import qualified Distribution.Simple.SetupHooks.Rule as Rule
import Distribution.Types.Component

import qualified Data.Graph as Graph
import Data.List
  ( intercalate
  )
import qualified Data.List.NonEmpty as NE
import qualified Data.Tree as Tree

import System.FilePath (normalise, (</>))

--------------------------------------------------------------------------------

-- | An error involving the @SetupHooks@ module of a package with
-- Hooks build-type.
data SetupHooksException
  = -- | Cannot apply a diff to a component in a per-component configure hook.
    CannotApplyComponentDiff CannotApplyComponentDiffReason
  | -- | An error with pre-build rules.
    RulesException RulesException
  deriving (Show)

-- | AN error involving the @Rules@ in the @SetupHooks@ module of a
-- package with the Hooks build-type.
data RulesException
  = -- | There are cycles in the dependency graph of fine-grained rules.
    CyclicRuleDependencies
      (NE.NonEmpty (Rule, [Graph.Tree Rule]))
  | -- | When executing fine-grained rules compiled into the external hooks
    -- executable, we failed to find dependencies of a rule.
    CantFindSourceForRuleDependencies
      Rule
      (NE.NonEmpty Rule.Location)
      -- ^ missing dependencies
  | -- | When executing fine-grained rules compiled into the external hooks
    -- executable, a rule failed to generate the outputs it claimed it would.
    MissingRuleOutputs
      Rule
      (NE.NonEmpty Rule.Location)
      -- ^ missing outputs
  | -- | An invalid reference to a rule output, e.g. an out-of-range
    -- index.
    InvalidRuleOutputIndex
      RuleId
      -- ^ rule
      RuleId
      -- ^ dependency
      (NE.NonEmpty Rule.Location)
      -- ^ outputs of dependency
      Word
      -- ^ the invalid index
  | -- | A duplicate 'RuleId' in the construction of pre-build rules.
    DuplicateRuleId !RuleId !Rule !Rule
  | InvalidLocations
     (NE.NonEmpty InvalidRuleLocations)

deriving instance Show RulesException

data InvalidRuleLocations
  = InvalidRuleLocations
  { invalidRuleLocationsRule :: Rule
  , invalidRuleInputLocations :: [(Location, InvalidRuleInputLocationReason)]
  , invalidRuleOutputLocations :: [(Location, InvalidRuleOutputLocationReason)]
  } -- invariant: at least one of the lists is non-empty

  deriving (Show)

data InvalidRuleInputLocationReason
  = InputLocationOutsidePackage
  | InputLocationNotRelative
  deriving (Show)

data InvalidRuleOutputLocationReason
  = OutputLocationOutsidePackage
  | OutputLocationOutsideDesignatedDir
  | OutputLocationNotRelative
  deriving (Show)
>>>>>>> dec30b202 (WIP: SetupHooks: error on invalid rule locations)

data CannotApplyComponentDiffReason
  = MismatchedComponentTypes Component Component
  | IllegalComponentDiff Component (NE.NonEmpty IllegalComponentDiffReason)
  deriving (Show)

data IllegalComponentDiffReason
  = CannotChangeName
  | CannotChangeComponentField String
  | CannotChangeBuildInfoField String
  deriving (Show)

setupHooksExceptionCode :: SetupHooksException -> Int
setupHooksExceptionCode = \case
  CannotApplyComponentDiff rea ->
    cannotApplyComponentDiffCode rea
  RulesException rea ->
    rulesExceptionCode rea

rulesExceptionCode :: RulesException -> Int
rulesExceptionCode = \case
  CyclicRuleDependencies{} -> 9077
  CantFindSourceForRuleDependencies{} -> 1071
  MissingRuleOutputs{} -> 3498
  InvalidRuleOutputIndex{} -> 1173
  DuplicateRuleId{} -> 7717
  InvalidLocations{} -> 7717

setupHooksExceptionMessage :: SetupHooksException -> String
setupHooksExceptionMessage = \case
  CannotApplyComponentDiff reason ->
    cannotApplyComponentDiffMessage reason
  RulesException reason ->
    rulesExceptionMessage reason

rulesExceptionMessage :: RulesException -> String
rulesExceptionMessage = \case
  CyclicRuleDependencies cycles ->
    unlines $
      ("Hooks: cycle" ++ plural ++ " in dependency structure of rules:")
        : map showCycle (NE.toList cycles)
    where
      plural :: String
      plural
        | NE.length cycles >= 2 =
            "s"
        | otherwise =
            ""
      showCycle :: (Rule, [Graph.Tree Rule]) -> String
      showCycle (r, rs) =
        unlines . map ("  " ++) . lines $
          Tree.drawTree $
            fmap showRule $
              Tree.Node r rs
  CantFindSourceForRuleDependencies _r deps ->
    unlines $
      ("Pre-build rules: can't find source for rule " ++ what ++ ":")
        : map (\d -> "  - " <> locPath d) depsL
    where
      depsL = NE.toList deps
      what
        | length depsL == 1 =
            "dependency"
        | otherwise =
            "dependencies"
  MissingRuleOutputs _r reslts ->
    unlines $
      ("Pre-build rule did not generate expected result" <> plural <> ":")
        : map (\res -> "  - " <> locPath res) resultsL
    where
      resultsL = NE.toList reslts
      plural
        | length resultsL == 1 =
            ""
        | otherwise =
            "s"
  InvalidRuleOutputIndex rId depRuleId outputs i -> unlines [header, body]
    where
      header = "Invalid index '" ++ show i ++ "' in dependency of " ++ show rId ++ "."
      nbOutputs = NE.length outputs
      body
        | (fromIntegral i :: Int) >= 0 =
            unwords
              [ "The dependency"
              , show depRuleId
              , "only has"
              , show nbOutputs
              , "output" ++ plural ++ "."
              ]
        | otherwise =
            "The index is too large."
      plural = if nbOutputs == 1 then "" else "s"
  DuplicateRuleId rId r1 r2 ->
    unlines $
      [ "Duplicate pre-build rule (" <> show rId <> ")"
      , "  - " <> showRule r1
      , "  - " <> showRule r2
      ]
  where
    showRule :: Rule -> String
    showRule (Rule{staticDependencies = deps, results = reslts}) =
      "Rule: " ++ showDeps deps ++ " --> " ++ showLocs (NE.toList reslts)
  InvalidLocations badRules ->
    unlines $
      ("Pre-build rules contain invalid " ++ what ++ " locations :")
      : map showBadRule badRulesL
    where
      badRulesL = NE.toList badRules
      what
        | badInputs && badOutputs
        = "input and output"
        | badInputs
        = "input"
        | otherwise
        = "output"
      badInputs = any (not . null . invalidRuleInputLocations) badRulesL
      badOutputs = any (not . null . invalidRuleOutputLocations) badRulesL
      showBadRule r =
        unlines $
          [ "  - rule dependency " ++ locPath loc ++ " " ++ invalidRuleInputLocationReason rea
          | (loc, rea) <- invalidRuleInputLocations r ]
          ++
          [ "  - rule output " ++ locPath loc ++ " " ++ invalidRuleOutputLocationReason rea
          | (loc, rea) <- invalidRuleOutputLocations r ]

locPath :: Location -> String
locPath (base, fp) = normalise $ base </> fp

showLocs :: [Location] -> String
showLocs locs = "[" ++ intercalate ", " (map locPath locs) ++ "]"

showDeps :: [Rule.Dependency] -> String
showDeps deps = "[" ++ intercalate ", " (map showDep deps) ++ "]"

showDep :: Rule.Dependency -> String
showDep = \case
  RuleDependency (RuleOutput{outputOfRule = rId, outputIndex = i}) ->
    "(" ++ show rId ++ ")[" ++ show i ++ "]"
  FileDependency loc -> locPath loc

invalidRuleInputLocationReason :: InvalidRuleInputLocationReason -> String
invalidRuleInputLocationReason = \ case
  InputLocationOutsidePackage -> "lies outside the package structure"
  InputLocationNotRelative -> "is not a relative path"

invalidRuleOutputLocationReason :: InvalidRuleOutputLocationReason -> String
invalidRuleOutputLocationReason = \ case
  OutputLocationOutsidePackage -> "lies outside the package structure"
  OutputLocationOutsideDesignatedDir -> "lies outside the designated output directories (autogen, build)"
  OutputLocationNotRelative -> "is not a relative path"

cannotApplyComponentDiffCode :: CannotApplyComponentDiffReason -> Int
cannotApplyComponentDiffCode = \case
  MismatchedComponentTypes{} -> 9491
  IllegalComponentDiff{} -> 7634

cannotApplyComponentDiffMessage :: CannotApplyComponentDiffReason -> String
cannotApplyComponentDiffMessage = \case
  MismatchedComponentTypes comp diff ->
    unlines
      [ "Hooks: mismatched component types in per-component configure hook."
      , "Trying to apply " ++ what ++ " diff to " ++ to ++ "."
      ]
    where
      what = case diff of
        CLib{} -> "a library"
        CFLib{} -> "a foreign library"
        CExe{} -> "an executable"
        CTest{} -> "a testsuite"
        CBench{} -> "a benchmark"
      to = case componentName comp of
        nm@(CExeName{}) -> "an " ++ showComponentName nm
        nm -> "a " ++ showComponentName nm
  IllegalComponentDiff comp reasons ->
    unlines $
      ("Hooks: illegal component diff in per-component pre-configure hook for " ++ what ++ ":")
        : map mk_rea (NE.toList reasons)
    where
      mk_rea err = "  - " ++ illegalComponentDiffMessage err ++ "."
      what = case componentName comp of
        CLibName LMainLibName -> "main library"
        nm -> showComponentName nm

illegalComponentDiffMessage :: IllegalComponentDiffReason -> String
illegalComponentDiffMessage = \case
  CannotChangeName ->
    "cannot change the name of a component"
  CannotChangeComponentField fld ->
    "cannot change component field '" ++ fld ++ "'"
  CannotChangeBuildInfoField fld ->
    "cannot change BuildInfo field '" ++ fld ++ "'"
