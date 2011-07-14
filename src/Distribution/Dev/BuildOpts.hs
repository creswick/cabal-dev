module Distribution.Dev.BuildOpts
    ( getBuildArgs
    , actions
    )
where

import Distribution.Dev.GhcArgs ( extractGHCArgs, formatGHCArgs )
import Control.Applicative ( (<$>) )
import Distribution.Dev.Command ( CommandActions(..), CommandResult(..) )
import Distribution.Dev.Flags ( Config, getVerbosity, cfgCabalInstall )
import Distribution.Simple.Program ( getProgramOutput )
import System.Console.GetOpt  ( OptDescr )

import qualified Distribution.Dev.CabalInstall as CI
import Distribution.Dev.InvokeCabal ( cabalArgs )

actions :: CommandActions
actions = CommandActions
              { cmdDesc = "Output the build arguments that Cabal would give to GHC."
              , cmdRun = \cfg _ args -> run cfg args
              , cmdOpts = [] :: [OptDescr ()]
              , cmdPassFlags = True
              }

run :: Config -> [String] -> IO CommandResult
run cfg args = do
  res <- getBuildArgs cfg args
  case res of
    Left err -> return $ CommandError err
    Right argLists ->
        do mapM_ (putStr . formatGHCArgs) argLists
           return CommandOk

getBuildArgs :: Config -> [String] -> IO (Either String [[String]])
getBuildArgs cfg args = do
  let v = getVerbosity cfg
  cabal <- CI.findOnPath v $ cfgCabalInstall cfg
  res <- cabalArgs cabal cfg CI.Build
  case res of
    Left err -> return $ Left err
    Right args' ->
        do let fakeBuildArgs = args' ++ args ++ ["--with-ghc=fake-ghc-cabal-dev"]

           -- Invoke "cabal build" with our argument-sniffing program
           -- acting as GHC
           Right . extractGHCArgs <$> getProgramOutput v cabal fakeBuildArgs
