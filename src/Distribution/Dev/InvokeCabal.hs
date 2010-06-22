module Distribution.Dev.InvokeCabal
    ( actions )
where

import Distribution.Dev.LocalRepo ( resolveSandbox, cabalConf )
import Distribution.Dev.Flags ( GlobalFlag )
import Distribution.Dev.InitPkgDb ( initPkgDb )
import Distribution.Dev.Command ( CommandActions(..), CommandResult(..) )
import System.Console.GetOpt ( OptDescr )
import System.Exit ( ExitCode(..) )
import System.Process ( proc, createProcess, waitForProcess )

actions :: String -> CommandActions
actions act = CommandActions
              { cmdDesc = "Invoke cabal-install with the \ 
                          \development configuration"
              , cmdRun = \flgs _ args -> invokeCabal flgs (act:args)
              , cmdOpts = [] :: [OptDescr ()]
              }

invokeCabal :: [GlobalFlag] -> [String] -> IO CommandResult
invokeCabal flgs args = do
  s <- resolveSandbox flgs
  initPkgDb s
  let cmd = proc "cabal" $ ("--config-file=" ++ cabalConf s):args
  (_,_,_, h) <- createProcess cmd
  res <- waitForProcess h
  case res of
    ExitSuccess -> return CommandOk
    ExitFailure code -> return $ CommandError $
                        "cabal-install failed with " ++ show code
