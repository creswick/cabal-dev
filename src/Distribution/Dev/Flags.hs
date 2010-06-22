{- Copyright (c) 2010 Galois, Inc. -}
module Distribution.Dev.Flags
    ( GlobalFlag(..)
    , globalOpts
    , parseGlobalFlags
    , helpRequested
    )
where

import System.Console.GetOpt ( OptDescr(..), ArgOrder(..), ArgDescr(..), getOpt' )

data GlobalFlag = Help
                | Verbose
                | Sandbox FilePath
                | CabalConf FilePath
                  deriving (Eq, Show)

globalOpts :: [OptDescr GlobalFlag]
globalOpts = [ Option "h?" ["help"] (NoArg Help) "Show help text"
             , Option "s" ["sandbox"] (ReqArg Sandbox "DIR")
               "The location of the development cabal sandbox (default: \ 
               \./cabal-dev)"
             , Option "c" ["config"] (ReqArg CabalConf "PATH")
               "The location of the cabal-install config file"
             , Option "v" ["verbose"] (NoArg Verbose) "Show debugging output"
             ]

parseGlobalFlags :: [String] -> ([GlobalFlag], [String], [String])
parseGlobalFlags args =
    let (flgs, args', unknown, errs) = getOpt' Permute globalOpts args
    in (flgs, args' ++ unknown, errs)

helpRequested :: [GlobalFlag] -> Bool
helpRequested = (Help `elem`)
