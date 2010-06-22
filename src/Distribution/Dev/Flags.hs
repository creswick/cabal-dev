{- Copyright (c) 2010 Galois, Inc. -}
module Distribution.Dev.Flags
    ( GlobalFlag(..)
    , globalOpts
    , parseGlobalFlags
    , helpRequested
    , getCabalConfig
    , getVerbosity
    )
where

import Data.Maybe ( listToMaybe )
import System.Console.GetOpt ( OptDescr(..), ArgOrder(..), ArgDescr(..), getOpt' )
import Paths_cabal_dev ( getDataFileName )
import Distribution.Verbosity ( Verbosity, normal, flagToVerbosity )
import Distribution.ReadE ( runReadE )

data GlobalFlag = Help
                | Verbose String
                | Sandbox FilePath
                | CabalConf FilePath
                  deriving (Eq, Show)

globalOpts :: [OptDescr GlobalFlag]
globalOpts = [ Option "h?" ["help"] (NoArg Help) "Show help text"
             , Option "s" ["sandbox"] (ReqArg Sandbox "DIR")
               "The location of the development cabal sandbox (default: \ 
               \./cabal-dev)"
             , Option "c" ["config"] (ReqArg CabalConf "PATH")
               "The location of the cabal-install config file (default: \ 
               \use included)"
             , Option "v" ["verbose"] (ReqArg Verbose "LEVEL")
               "Verbosity level: 0 (silent) - 3 (deafening)"
             ]

parseGlobalFlags :: [String] -> ([GlobalFlag], [String], [String])
parseGlobalFlags args =
    let (flgs, args', unknown, errs) = getOpt' Permute globalOpts args
    in (flgs, args' ++ unknown, errs)

helpRequested :: [GlobalFlag] -> Bool
helpRequested = (Help `elem`)

cabalConfigFlag :: [GlobalFlag] -> Maybe FilePath
cabalConfigFlag flgs = listToMaybe [p | CabalConf p <- flgs]

getCabalConfig :: [GlobalFlag] -> IO FilePath
getCabalConfig = maybe (getDataFileName "admin/cabal-config.in") return .
                 cabalConfigFlag

getVerbosity :: [GlobalFlag] -> Verbosity
getVerbosity flgs =
    case map (runReadE flagToVerbosity) [ s | Verbose s <- flgs ] of
      (Right v:_) -> v
      _           -> normal
