{-# LANGUAGE ExistentialQuantification #-}
module Distribution.Dev.Command
    ( CommandActions(..)
    , allCommandNames
    , Command(..)
    , CommandResult(..)
    , cmdName
    , globalUsage
    , runCmd
    )
where

import System.Environment ( getProgName )
import Distribution.Dev.Flags ( GlobalFlag, globalOpts, helpRequested )
import System.Console.GetOpt ( OptDescr(..), ArgOrder(..), usageInfo, getOpt )
import System.Exit ( exitFailure, exitSuccess )

data Command = MkRepo deriving (Enum, Bounded)

data CommandResult = CommandError String | CommandOk

data CommandActions
    = forall a . CommandActions
      { cmdDesc :: String
      , cmdRun :: [GlobalFlag] -> [a] -> [String] -> IO CommandResult
      , cmdOpts :: [OptDescr a]
      }

cmdName :: String -> Maybe Command
cmdName s = case s of
             "mk-repo" -> Just MkRepo
             _ -> Nothing

nameCmd :: Command -> String
nameCmd MkRepo = "mk-repo"

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
allCommandNames = map nameCmd [minBound..maxBound]

runCmd :: CommandActions -> [GlobalFlag] -> [String] -> IO ()
runCmd cmdAct flgs args
    | helpRequested flgs = showHelp
    | otherwise = do res <- run
                     case res of
                       CommandOk        -> exitSuccess
                       CommandError msg -> showError [msg]
    where
      showError msgs = do
        putStr $ unlines $ msgs ++ [cmdDesc cmdAct]
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
