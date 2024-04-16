

module CallCustomPp ( callCustomPp ) where

-- base
import System.Exit
  ( ExitCode(..) )

-- process
import System.Process
  ( readProcessWithExitCode )

--------------------------------------------------------------------------------

callCustomPp :: IO ()
callCustomPp = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode customPpName [] ""
  case exitCode of
    ExitSuccess ->
      putStr stdout
    ExitFailure {} ->
      error $ unlines
        [ customPpName ++ " failed with error code " ++ show exitCode
        , "stdout: " ++ stdout
        , "stderr: " ++ stderr ]

customPpName :: String
customPpName = "custom-build-tool"
