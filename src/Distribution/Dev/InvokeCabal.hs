module Distribution.Dev.InvokeCabal
    ( actions )
where

import Control.Arrow          ( left, right )
import Distribution.Verbosity ( Verbosity, showForCabal )
import System.Console.GetOpt  ( OptDescr )
import System.Exit            ( ExitCode(..) )
import System.Process         ( rawSystem )

import Distribution.Dev.Command            ( CommandActions(..)
                                           , CommandResult(..)
                                           )
import Distribution.Dev.Flags              ( GlobalFlag, getCabalConfig
                                           , getVerbosity
                                           )
import Distribution.Dev.InitPkgDb          ( initPkgDb )
import Distribution.Dev.RewriteCabalConfig ( rewriteCabalConfig )
import Distribution.Dev.Sandbox            ( resolveSandbox, cabalConf )

actions :: String -> CommandActions
actions act = CommandActions
              { cmdDesc = "Invoke cabal-install with the \ 
                          \development configuration"
              , cmdRun = \flgs _ args -> invokeCabal flgs (act:args)
              , cmdOpts = [] :: [OptDescr ()]
              , cmdPassFlags = True
              }

invokeCabal :: [GlobalFlag] -> [String] -> IO CommandResult
invokeCabal flgs args = do
  either (return . CommandError) (invokeCabalCfg args) =<< setup flgs

setup :: [GlobalFlag] -> IO (Either String (Verbosity, FilePath))
setup flgs = do
  s <- initPkgDb =<< resolveSandbox flgs
  cfgIn <- getCabalConfig flgs
  let cfgOut = cabalConf s
  cfgRes <- rewriteCabalConfig cfgIn cfgOut s
  let qualifyError err =
          "Error processing cabal config file " ++ cfgIn ++ ": " ++ err
  return $
         left qualifyError $
         right (const (getVerbosity flgs, cfgOut)) $
         cfgRes

invokeCabalCfg :: [String] -> (Verbosity, FilePath) -> IO CommandResult
invokeCabalCfg args (v, cfg) = do
  let args' = ("--config-file=" ++ cfg):
              ("--verbose=" ++ showForCabal v):
              args
  res <- rawSystem "cabal" args'
  return $ case res of
             ExitSuccess      -> CommandOk
             ExitFailure code -> CommandError $
                                 "cabal-install failed with " ++ show code
