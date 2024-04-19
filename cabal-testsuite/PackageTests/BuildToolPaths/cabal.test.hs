import Test.Cabal.Prelude

-- Test that build-tool-depends executables are correctly provisioned
-- in all circumstances.
main = cabalTest $ do
  --skipIfAllCabalVersion "< 3.11"
  -- At build-time:
  --
  --  - in a pre-build hook
  --  - in a Template Haskell splice
  cabal "build" [ "all", "--enable-tests", "--enable-benchmarks" ]
  -- At runtime of a test-suite
  cabal "test" []
  -- At runtime of a benchmark
  cabal "bench" []
  -- At runtime of an executable
  cabal "run" [ "package-with-build-tools-exe" ]
