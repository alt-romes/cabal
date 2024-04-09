{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StaticPointers #-}

module SetupHooks where

import Distribution.Simple.LocalBuildInfo (interpretSymbolicPathLBI)
import Distribution.Simple.SetupHooks
import Distribution.Simple.Utils ( rewriteFileEx, warn )

import Data.Foldable ( for_ )
import qualified Data.List.NonEmpty as NE ( NonEmpty(..) )
import Data.Traversable ( for )

import System.FilePath
  ( (<.>), (</>) )

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { buildHooks =
        noBuildHooks
          { preBuildComponentRules = Just $ rules $ static preBuildRules
          }
    }

-- Register three rules:
--
-- r1: B --> C
-- r2: A --> B
-- r3: C --> D
--
-- and check that we run them in dependency order, i.e. r2, r1, r3.
preBuildRules :: PreBuildComponentInputs -> RulesM ()
preBuildRules (PreBuildComponentInputs { buildingWhat = what, localBuildInfo = lbi, targetInfo = tgt }) = mdo
  let verbosity = buildingWhatVerbosity what
      clbi = targetCLBI tgt
      i = interpretSymbolicPathLBI lbi
      autogenDir = i (autogenComponentModulesDir lbi clbi)

      mkAction =
        mkCommand (static Dict) $ static (\ (dir, verb, (inMod, outMod)) -> do
          warn verb $ "Running rule: " ++ inMod ++ " --> " ++ outMod
          let loc = dir </> outMod <.> "hs"
          rewriteFileEx verb loc $
            "module " ++ outMod ++ " where { import " ++ inMod ++ " }"
        )

      actionArg inMod outMod = (autogenDir, verbosity, (inMod, outMod))

      mkRule action input outMod =
        staticRule action
          [ input ]
          ( ( autogenDir, outMod <.> "hs" ) NE.:| [] )

  r1 <- registerRule "r1" $ mkRule (mkAction (actionArg "B" "C")) (RuleDependency $ RuleOutput r2 0) "C" -- B --> C
  r2 <- registerRule "r2" $ mkRule (mkAction (actionArg "A" "B")) (FileDependency (".", "A.hs"))     "B" -- A --> B
  r3 <- registerRule "r3" $ mkRule (mkAction (actionArg "C" "D")) (RuleDependency $ RuleOutput r1 0) "D" -- C --> D
  return ()
