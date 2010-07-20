{-# LANGUAGE ExistentialQuantification #-}
module Distribution.Dev.Command
    ( CommandActions(..)
    , CommandResult(..)
    )
where

import System.Console.GetOpt ( OptDescr(..) )

import Distribution.Dev.Flags ( Config )

data CommandResult = CommandError String | CommandOk

data CommandActions
    = forall a . CommandActions
      { cmdDesc :: String
      , cmdRun :: Config -> [a] -> [String] -> IO CommandResult
      , cmdOpts :: [OptDescr a]
      , cmdPassFlags :: Bool
      }
