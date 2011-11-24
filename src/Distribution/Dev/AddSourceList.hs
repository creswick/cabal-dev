{- Copyright (c) 2011 Galois, Inc. -}
{-|
add-source-list command

Puts local source packages into a repository readable by cabal-install
-}
module Distribution.Dev.AddSourceList ( actions) where

import Distribution.Dev.AddSource (addSources)
import Distribution.Dev.Command ( CommandActions(..), CommandResult(..) )
import Distribution.Dev.Flags   ( Config, getVerbosity )
import Distribution.Simple.Utils ( debug )
import System.Console.GetOpt                 ( OptDescr(..) )

import System.FilePath ( takeDirectory, (</>) )

actions :: CommandActions
actions = CommandActions
            { cmdDesc = "Add local packages listed in a file"
            , cmdRun = \cfg _ -> addSourcesFromFiles cfg
            , cmdOpts = [] :: [OptDescr ()]
            , cmdPassFlags = False
            }

addSourcesFromFiles :: Config -> [String] -> IO CommandResult
addSourcesFromFiles _    [] = return $ CommandError "No source file supplied"
addSourcesFromFiles flgs fns = do
  let v = getVerbosity flgs
  debug v $ "Adding source files" ++ show fns
  localDeps <- mapM localPathsFromFile fns
  doCommands (map (addSources flgs) localDeps)
  where
    doCommands :: [IO CommandResult] -> IO CommandResult
    doCommands [] = return CommandOk
    doCommands (c:commands) = do
      r <- c
      case r of
        CommandOk -> doCommands commands
        e         -> return e

    localPathsFromFile :: FilePath -> IO [String]
    localPathsFromFile file = do
      fileRelativeDeps <- (fmap lines . readFile) file
      return $ map dirname fileRelativeDeps
      where 
        dirname s = (takeDirectory file) </> s

