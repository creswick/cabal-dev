{- Copyright (c) 2010 Galois, Inc -}
module Main
    ( main )
where

import Data.Maybe ( listToMaybe )
import Data.Version ( showVersion )
import Control.Monad ( unless )
import System.Exit ( exitWith, ExitCode(..) )
import System.Environment ( getArgs, getProgName )
import System.Console.GetOpt ( usageInfo, getOpt, ArgOrder(Permute) )
import Distribution.Simple.Utils ( cabalVersion, debug )
import Distribution.Text ( display )

import Distribution.Dev.Command ( CommandActions(..), CommandResult(..) )
import Distribution.Dev.Flags ( parseGlobalFlags, helpRequested, globalOpts
                              , GlobalFlag(Version), getOpt'', fromFlags
                              , getVerbosity, Config
                              )
import qualified Distribution.Dev.AddSource as AddSource
import qualified Distribution.Dev.Ghci as Ghci
import qualified Distribution.Dev.InvokeCabal as InvokeCabal
import qualified Distribution.Dev.InstallDependencies as InstallDeps
import Paths_cabal_dev ( version )

allCommands :: [(String, CommandActions)]
allCommands = [ ("add-source", AddSource.actions)
              , ("install-deps", InstallDeps.actions)
              , ("ghci", Ghci.actions)
              , cabal "build"
              , cabal "clean"
              , cabal "configure"
              , cabal "copy"
              , cabal "fetch"
              , cabal "haddock"
              , cabal "info"
              , cabal "init"
              , cabal "install"
              , cabal "list"
              , cabal "register"
              , cabal "unpack"
              , cabal "update"
              , cabal "hscolour"
              , cabal "sdist"
              ]
    where
      cabal s = (s, InvokeCabal.actions s)

printVersion :: IO ()
printVersion = do
  putStr versionString
  exitWith ExitSuccess

versionString :: String
versionString = unlines $
                [ "cabal-dev " ++ showVersion version
                , "built with Cabal " ++ display cabalVersion
                ]

printNumericVersion :: IO ()
printNumericVersion = do
  putStrLn $ showVersion version
  exitWith ExitSuccess

main :: IO ()
main = do
  (globalFlags, args, errs) <- parseGlobalFlags `fmap` getArgs
  unless (null errs) $ do
         mapM_ putStrLn errs
         putStr =<< globalUsage
         exitWith (ExitFailure 1)

  case [f|(Version f) <- globalFlags] of
    (True:_) -> printNumericVersion
    (False:_) -> printVersion
    [] -> return ()

  let cfg = fromFlags globalFlags
  debug (getVerbosity cfg) versionString

  case args of
    (name:args') ->
        case nameCmd name of
          Just cmdAct | helpRequested globalFlags ->
                          do putStrLn $ cmdDesc cmdAct
                             putStr =<< globalUsage
                             exitWith ExitSuccess
                      | otherwise -> runCmd cmdAct cfg args'

          Nothing -> do putStrLn $ "Unknown command: " ++ show name
                        putStr =<< globalUsage
                        exitWith (ExitFailure 1)
    _ | helpRequested globalFlags -> do
              putStr =<< globalUsage
              exitWith ExitSuccess
      | otherwise -> do
              putStrLn "Missing command name"
              putStr =<< globalUsage
              exitWith (ExitFailure 1)

globalUsage :: IO String
globalUsage = do
  progName <- getProgName
  let preamble =
          unlines $
          [ ""
          , "Usage: " ++ progName ++ " <command>"
          , ""
          , "Where <command> is one of:"
          ] ++ map ("  " ++) allCommandNames ++
          [ ""
          , "Options:"
          ]
  return $ usageInfo preamble globalOpts

allCommandNames :: [String]
allCommandNames = map fst allCommands

nameCmd :: String -> Maybe CommandActions
nameCmd s = listToMaybe [a | (n, a) <- allCommands, n == s]

runCmd :: CommandActions -> Config -> [String] -> IO ()
runCmd cmdAct cfg args =
    do res <- run
       case res of
         CommandOk        -> exitWith ExitSuccess
         CommandError msg -> showError [msg]
    where
      showError msgs = do
        putStr $ unlines $ "FAILED:":msgs ++ [replicate 50 '-', cmdDesc cmdAct]
        putStr =<< globalUsage
        exitWith (ExitFailure 1)

      run = case cmdAct of
              (CommandActions _ r o passFlags) ->
                  let (cmdFlags, cmdArgs, cmdErrs) =
                          if passFlags
                          then getOpt'' o args
                          else getOpt Permute o args
                  in if null cmdErrs
                     then r cfg cmdFlags cmdArgs
                     else showError cmdErrs
