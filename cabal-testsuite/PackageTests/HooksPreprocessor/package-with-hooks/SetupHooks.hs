{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers #-}

{-# OPTIONS_GHC -Wall #-}

module SetupHooks ( setupHooks ) where

-- base
import Control.Monad.IO.Class
  ( liftIO )
import Data.Foldable
  ( for_ )
import qualified Data.List.NonEmpty as NE
  ( singleton )
import Data.Maybe
  ( catMaybes )
import Data.Traversable
  ( for )

-- Cabal-hooks
import Distribution.Simple.SetupHooks

-- Cabal
import Distribution.ModuleName
  ( ModuleName )
import Distribution.Simple.Flag
  ( fromFlag )
import Distribution.Simple.LocalBuildInfo
  ( mbWorkDirLBI )
import Distribution.Simple.Program
  ( configureProgram, runProgramCwd )
import Distribution.Simple.Program.Db
  ( lookupProgram )
import Distribution.Simple.Utils
  ( createDirectoryIfMissingVerbose, findFileCwdWithExtension' )
import Distribution.Types.Component
  ( componentBuildInfo )
import qualified Distribution.Types.LocalBuildConfig as LBC
  ( withPrograms )
import Distribution.Types.LocalBuildInfo
  ( withPrograms )
import Distribution.Types.VersionRange
  ( anyVersion )
import Distribution.Utils.Path
  ( SymbolicPath, FileOrDir(Dir), CWD, Pkg
  , interpretSymbolicPath, getSymbolicPath, moduleNameSymbolicPath
  )
import Distribution.Utils.ShortText
  ( toShortText )

-- containers
import qualified Data.Map as Map
  ( singleton )

-- directory
import System.Directory
  ( getCurrentDirectory )

-- filepath
import System.FilePath
  ( (</>), replaceExtension, takeDirectory )

--------------------------------------------------------------------------------

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { buildHooks =
      noBuildHooks
        { preBuildComponentRules =
            Just $ rules (static ()) preBuildRules
        }
    }

customPpName :: String
customPpName = "custom-build-tool"

-- | Runs the custom-build-tool preprocessor on all .hs-custompp files.
preBuildRules :: PreBuildComponentInputs -> RulesM ()
preBuildRules
  PreBuildComponentInputs
    { buildingWhat   = what
    , localBuildInfo = lbi
    , targetInfo     = tgt
    }
  = do
      let verbosity = buildingWhatVerbosity what
          comp = targetComponent tgt
          compNm = componentName comp
          clbi = targetCLBI tgt
          bi = componentBuildInfo comp
          progDb = withPrograms lbi
          mbWorkDir = mbWorkDirLBI lbi

    -- 1. Look up the custom-build-tool preprocessor.
      let customPpProg = simpleProgram customPpName
          mbCustomPp = lookupProgram customPpProg progDb
          customPp = case mbCustomPp of
            Just pp -> pp
            Nothing ->
              error $
                unlines
                  [ "package-with-hooks: could not find " ++ show customPpName ++ " pre-processor in the program database."
                  , "Component: " ++ show compNm ]

    -- 2. Create a command to run this preprocess, passing input and output file locations.
      let
        ppCmd :: Location -> Location
              -> Command ( Verbosity, Maybe (SymbolicPath CWD (Dir Pkg)), ConfiguredProgram, Location, Location ) ( IO () )
        ppCmd i o =
          mkCommand ( static Dict ) ( static ppModule )
            ( verbosity, mbWorkDir, customPp,  i, o )

    -- 3. Get all modules listed in the package description for this component.
      let mods = componentModules comp

    -- 4. Search whether any such module exists in the source tree
    --    with the "custom-pp" extension.
      let searchDirs = hsSourceDirs bi
      ppMbMods <-
        liftIO $
          for mods $ \ md -> do
            mbPath <- findFileCwdWithExtension' mbWorkDir [ "hs-custompp" ] searchDirs
                        ( moduleNameSymbolicPath md )
            case mbPath of
              Just ( base, rel ) ->
                return $
                  Just
                    ( md, ( getSymbolicPath base, getSymbolicPath rel ) )
              Nothing ->
                return Nothing
      let ppMods = catMaybes ppMbMods
      liftIO $ putStrLn $ unlines $
        "package-with-hooks: hs-custompp modules:"
        : ( map ( \ m -> "  - " ++ show m ) mods )
    -- TODO: declare the corresponding monitored files corresponding to the
    -- above search (it would be nice to be able to use findFileWithExtensionMonitored).

    -- 5. Declare a rule for each custom-pp module that runs the pre-processor.
      for_ ppMods $ \ ( md, inputLoc@( _inputBaseDir, inputRelPath ) ) -> do
        let outputBaseLoc = getSymbolicPath $ autogenComponentModulesDir lbi clbi
            outputLoc = ( outputBaseLoc, replaceExtension inputRelPath "hs" )
        registerRule_ ( toShortText $ show md ) $
          staticRule ( ppCmd inputLoc outputLoc ) [] ( NE.singleton outputLoc )

ppModule :: ( Verbosity, Maybe (SymbolicPath CWD (Dir Pkg)), ConfiguredProgram, Location, Location ) -> IO ()
ppModule ( verbosity, mbWorkDir, customPp, ( inputBaseDir, inputRelPath ), ( outputBaseDir, outputRelPath ) ) = do
  let inputPath  = inputBaseDir </> inputRelPath
      outputPath = outputBaseDir </> outputRelPath
  createDirectoryIfMissingVerbose verbosity True $ takeDirectory outputPath
  runProgramCwd verbosity mbWorkDir customPp [ inputPath, outputPath ]

componentModules :: Component -> [ ModuleName ]
componentModules comp = libMods ++ otherModules ( componentBuildInfo comp )
  where
    libMods = case comp of
      CLib lib -> exposedModules lib
      _        -> []
  -- TODO: this doesn't take into account things like hs-boot or hsig files,
  -- but that's okay for this simple test.
