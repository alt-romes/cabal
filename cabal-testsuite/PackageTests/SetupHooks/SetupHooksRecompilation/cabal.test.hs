import Test.Cabal.Prelude

main = cabalTest $ do
  recordMode DoNotRecord $ do
    cabal "build" []
    liftIO $ appendFile "SetupHooks.hs" "definitely-wrong!"
    -- If this doesn't fail, it's because we didn't re-build.
    fails $ cabal "build" []

