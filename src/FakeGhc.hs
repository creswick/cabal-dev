{-|

Program to invoke "as ghc" in order to snoop the arguments that
cabal-install passes, so that we can use them to run ghci. This trick
was lifted from Leksah.

-}
module Main ( main ) where

import System.Environment ( getArgs )
import Distribution.Dev.GhcArgs ( formatGHCArgs )

-- |Take the command line arguments and format them in an
-- easily-parsed way.
main :: IO ()
main = putStr . formatGHCArgs =<< getArgs
