{- Copyright (c) 2010 Galois, Inc -}
module Main
    ( main )
where

import Data.Maybe ( listToMaybe )
import Control.Monad ( unless )
import System.Exit ( exitWith, ExitCode(..) )
import System.Environment ( getArgs, getProgName )
import System.Console.GetOpt ( usageInfo, getOpt, ArgOrder(Permute), getOpt' )

import Distribution.Dev.Command ( CommandActions(..), CommandResult(..) )
import Distribution.Dev.Flags ( parseGlobalFlags, helpRequested, globalOpts, GlobalFlag )
import qualified Distribution.Dev.AddSource as AddSource
import qualified Distribution.Dev.InvokeCabal as InvokeCabal

allCommands :: [(String, CommandActions)]
allCommands = [ ("add-source", AddSource.actions)
              , cabal "install"
              , cabal "update"
              , cabal "list"
              , cabal "info"
              , cabal "fetch"
              , cabal "unpack"
              , cabal "configure"
              , cabal "build"
              , cabal "register"
              , cabal "clean"
              , cabal "haddock"
              , cabal "init"
              , cabal "hscolour"
              ]
    where
      cabal s = (s, InvokeCabal.actions s)

main :: IO ()
main = do
  (globalFlags, args, errs) <- parseGlobalFlags `fmap` getArgs
  unless (null errs) $ do
         mapM_ putStrLn errs
         putStr =<< globalUsage
         exitWith (ExitFailure 1)

  case args of
    (name:args') ->
        case nameCmd name of
          Just cmdAct -> runCmd cmdAct globalFlags args'
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

runCmd :: CommandActions -> [GlobalFlag] -> [String] -> IO ()
runCmd cmdAct flgs args
    | helpRequested flgs = showHelp
    | otherwise = do res <- run
                     case res of
                       CommandOk        -> exitWith ExitSuccess
                       CommandError msg -> showError [msg]
    where
      showError msgs = do
        putStr $ unlines $ "FAILED:":msgs ++ [replicate 50 '-', cmdDesc cmdAct]
        putStr =<< globalUsage
        exitWith (ExitFailure 1)

      showHelp = do
        putStrLn $ cmdDesc cmdAct
        putStr =<< globalUsage
        exitWith ExitSuccess

      run = case cmdAct of
              (CommandActions _ r o passFlags) ->
                  let (cmdFlags, cmdArgs, cmdErrs) =
                          if passFlags
                          then let (a, b, c, d) = getOpt' Permute o args
                               in (a, b ++ c, d)
                          else getOpt Permute o args
                  in if null cmdErrs
                     then r flgs cmdFlags cmdArgs
                     else showError cmdErrs
