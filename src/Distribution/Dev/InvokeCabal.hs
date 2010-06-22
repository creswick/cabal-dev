module Distribution.Dev.InvokeCabal
    ( actions )
where

import Control.Arrow ( left, right )
import Distribution.Dev.LocalRepo ( resolveSandbox, cabalConf )
import Distribution.Dev.Flags ( GlobalFlag, getCabalConfig )
import Distribution.Dev.InitPkgDb ( initPkgDb )
import Distribution.Dev.RewriteCabalConfig ( rewriteCabalConfig )
import Distribution.Dev.Command ( CommandActions(..), CommandResult(..) )
import System.Console.GetOpt ( OptDescr )
import System.Exit ( ExitCode(..) )
import System.Process ( rawSystem )

actions :: String -> CommandActions
actions act = CommandActions
              { cmdDesc = "Invoke cabal-install with the \ 
                          \development configuration"
              , cmdRun = \flgs _ args -> invokeCabal flgs (act:args)
              , cmdOpts = [] :: [OptDescr ()]
              }

invokeCabal :: [GlobalFlag] -> [String] -> IO CommandResult
invokeCabal flgs args = do
  either (return . CommandError) (invokeCabalCfg args) =<< setup flgs

setup :: [GlobalFlag] -> IO (Either String FilePath)
setup flgs = do
  s <- resolveSandbox flgs
  initPkgDb s
  cfgIn <- getCabalConfig flgs
  let cfgOut = cabalConf s
  cfgRes <- rewriteCabalConfig cfgIn cfgOut
  let qualifyError err =
          "Error processing cabal config file " ++ cfgIn ++ ": " ++ err
  return $ left qualifyError $ right (const cfgOut) $ cfgRes

invokeCabalCfg :: [String] -> FilePath -> IO CommandResult
invokeCabalCfg args cfg = do
  res <- rawSystem "cabal" $ ("--config-file=" ++ cfg):args
  return $ case res of
             ExitSuccess      -> CommandOk
             ExitFailure code -> CommandError $
                                 "cabal-install failed with " ++ show code
