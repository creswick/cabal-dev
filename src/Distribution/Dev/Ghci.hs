module Distribution.Dev.Ghci
   ( actions )
where

import Control.Arrow ( second )
import Distribution.Dev.Command ( CommandActions(..), CommandResult(..) )
import Distribution.Dev.Flags ( Config, getVerbosity, cfgCabalInstall )
import Distribution.Simple.Program ( emptyProgramConfiguration
                                   , runProgram
                                   , requireProgram
                                   , getProgramOutput
                                   , ghcProgram
                                   )
import System.Console.GetOpt  ( OptDescr )

import qualified Distribution.Dev.CabalInstall as CI
import Distribution.Dev.InvokeCabal ( cabalArgs )

actions :: CommandActions
actions = CommandActions
              { cmdDesc = "Run ghci configured as per the specified cabal file."
              , cmdRun = \cfg _ args -> invokeGhci cfg args
              , cmdOpts = [] :: [OptDescr ()]
              , cmdPassFlags = True
              }

invokeGhci :: Config -> [String] -> IO CommandResult
invokeGhci cfg args = do
  let v = getVerbosity cfg
  cabal <- CI.findOnPath v $ cfgCabalInstall cfg
  res <- cabalArgs cabal cfg CI.Build
  case res of
    Left err ->
        return $ CommandError err
    Right args' ->
        do (ghc, _) <- requireProgram v ghcProgram emptyProgramConfiguration

           -- Invoke "cabal build" with our argument-sniffing program
           -- acting as GHC
           out <- getProgramOutput v cabal $
                  args' ++ args ++ ["--with-ghc=fake-ghc-cabal-dev"]

           -- Use the arguments that cabal-install passed to GHC to
           -- invoke ghci instead
           runProgram v ghc $ "--interactive" : head (extractGHCArgs out)
           return CommandOk

extractGHCArgs :: String -> [[String]]
extractGHCArgs = go . lines
    where
      go [] = []
      go ls = let (args, rest) = findCabalArgs ls
              in args:go rest

      findCabalArgs = second (drop 1) .
                      break (isMarker "End") .
                      drop 1 .
                      dropWhile (not . isMarker "Start")

      isMarker s = (== ("== GHC Arguments: " ++ s ++ " =="))
