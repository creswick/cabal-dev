{-|

Program to invoke "as ghc" in order to snoop the arguments that
cabal-install passes, so that we can use them to run ghci. This trick
was lifted from Leksah.

-}
module Main ( main ) where

import System.Environment ( getArgs )

-- |Take the command line arguments and format them in an
-- easily-parsed way.
main :: IO ()
main = putStr . unlines . processArgs =<< getArgs

-- |Format the arguments, bracketed by start end end markers, in a way
-- that can be easily parsed.
--
-- Each argument is on its own line to avoid having to escape/parse
-- strings. This assumes that there are no newlines in the arguments.
processArgs :: [String] -> [String]
processArgs = (++ marker "End") . (marker "Start" ++) . removeMake
    where
      -- Remove the --make flag that cabal-install passes.
      removeMake = filter (/= "--make")

-- |Format a start or end marker
--
-- This is used to extract GHC arguments out of the output from
-- cabal-install, where there may be multiple invocations of GHC as
-- well as much other output.
marker :: String -> [String]
marker = return . showString "== GHC Arguments: " . (++ " ==")