{- Copyright (c) 2010 Galois, Inc -}
{-# LANGUAGE ExistentialQuantification #-}
module Main
    ( main )
where

import Data.Maybe ( listToMaybe )
import Control.Monad ( unless )
import System.Exit ( exitFailure, exitSuccess )
import System.Environment ( getArgs, getProgName )
import System.Console.GetOpt ( usageInfo, getOpt, ArgOrder(Permute) )

import Distribution.Dev.Command ( CommandActions(..), CommandResult(..) )
import Distribution.Dev.Flags ( parseGlobalFlags, helpRequested, globalOpts, GlobalFlag )
import qualified Distribution.Dev.MkRepo as MkRepo

allCommands :: [(String, CommandActions)]
allCommands = [("mk-repo", MkRepo.actions)]

main :: IO ()
main = do
  (globalFlags, args, errs) <- parseGlobalFlags `fmap` getArgs
  unless (null errs) $ do
         mapM_ putStrLn errs
         putStr =<< globalUsage
         exitFailure

  case args of
    (name:args') ->
        case nameCmd name of
          Just cmdAct -> runCmd cmdAct globalFlags args'
          Nothing -> do putStrLn $ "Unknown command: " ++ show name
                        putStr =<< globalUsage
                        exitFailure
    _ | helpRequested globalFlags -> do
              putStr =<< globalUsage
              exitSuccess
      | otherwise -> do
              putStrLn "Missing command name"
              putStr =<< globalUsage
              exitFailure

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
                       CommandOk        -> exitSuccess
                       CommandError msg -> showError [msg]
    where
      showError msgs = do
        putStr $ unlines $ "FAILED:":msgs ++ [replicate 50 '-', cmdDesc cmdAct]
        putStr =<< globalUsage
        exitFailure

      showHelp = do
        putStrLn $ cmdDesc cmdAct
        putStr =<< globalUsage
        exitSuccess

      run = case cmdAct of
              (CommandActions _ r o) ->
                  let (cmdFlags, cmdArgs, cmdErrs) =
                          getOpt Permute o args
                  in if null cmdErrs
                     then r flgs cmdFlags cmdArgs
                     else showError cmdErrs
