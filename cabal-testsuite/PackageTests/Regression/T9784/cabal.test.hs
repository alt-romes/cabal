import Test.Cabal.Prelude
-- Test that, despite not adding the inplace directory to the executable RPATH,
-- the inplace executable can be run with `cabal run`.
-- Don't test on Windows, which doesn't support RPATH.
main = cabalTest $ do
  skipIfWindows
  cabal "configure" ["--enable-executable-dynamic"]
  cabal "build" []
  -- This should succeed because, despite T4025, cabal run should augment the
  -- LD_LIBRARY_PATH with the inplace library
  cabal "run" []
