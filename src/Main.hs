{- Copyright (c) 2010 Galois, Inc -}
{-# LANGUAGE ExistentialQuantification #-}
module Main
    ( main )
where

import Control.Monad ( unless )
import System.Exit ( exitFailure, exitSuccess )
import System.Environment ( getArgs )

import Distribution.Dev.Command ( globalUsage, cmdName, Command(..), runCmd )
import Distribution.Dev.Flags ( parseGlobalFlags, helpRequested )
import Distribution.Dev.MkRepo ( mkRepoAct )

main :: IO ()
main = do
  (globalFlags, args, errs) <- parseGlobalFlags `fmap` getArgs
  unless (null errs) $ do
         mapM_ putStrLn errs
         putStr =<< globalUsage
         exitFailure

  case args of
    (name:args') ->
        case cmdName name of
          Just cmd -> do
              let cmdAct = case cmd of
                             MkRepo -> mkRepoAct

              runCmd cmdAct globalFlags args'
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
