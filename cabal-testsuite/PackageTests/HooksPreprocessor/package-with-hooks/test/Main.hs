{-# LANGUAGE TemplateHaskell #-}

module Main ( main ) where

-- template-haskell
import Language.Haskell.TH
  ( runIO )

-- package-with-hooks
import CallCustomPp
  ( callCustomPp )

--------------------------------------------------------------------------------

-- Check that we can invoke the custom preprocessor, and that it finds its
-- data directory, both at compile-time and at run-time.

$( do
    runIO callCustomPp
    return []
  )

main :: IO ()
main = callCustomPp
