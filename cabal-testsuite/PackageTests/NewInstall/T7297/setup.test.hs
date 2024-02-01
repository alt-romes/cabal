import Test.Cabal.Prelude

main = cabalTest $
  cabal "install" ["--enable-executable-dynamic"]

