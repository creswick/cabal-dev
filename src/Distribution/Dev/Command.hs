{-# LANGUAGE ExistentialQuantification #-}
module Distribution.Dev.Command
    ( CommandActions(..)
    , CommandResult(..)
    )
where

import Distribution.Dev.Flags ( GlobalFlag )
import System.Console.GetOpt ( OptDescr(..) )

data CommandResult = CommandError String | CommandOk

data CommandActions
    = forall a . CommandActions
      { cmdDesc :: String
      , cmdRun :: [GlobalFlag] -> [a] -> [String] -> IO CommandResult
      , cmdOpts :: [OptDescr a]
      , cmdPassFlags :: Bool
      }
