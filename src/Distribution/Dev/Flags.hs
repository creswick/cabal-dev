{- Copyright (c) 2010 Galois, Inc. -}
{-# LANGUAGE CPP #-}
module Distribution.Dev.Flags
    ( GlobalFlag(..)
    , globalOpts
    , parseGlobalFlags
    , helpRequested
    , getCabalConfig
    , getVerbosity
    )
where

#ifndef MIN_VERSION_Cabal
#define MIN_VERSION_Cabal(a,b,c) 1
#endif

import Data.Maybe             ( listToMaybe )
#if MIN_VERSION_Cabal(1,4,0)
import Distribution.ReadE     ( runReadE )
#elif MIN_VERSION_Cabal(1,2,0)
#else
#error Unsupported cabal version
#endif
import Distribution.Verbosity ( Verbosity, normal, flagToVerbosity )
import Paths_cabal_dev        ( getDataFileName )
import System.Console.GetOpt  ( OptDescr(..), ArgOrder(..), ArgDescr(..)
                              , getOpt'
                              )

data GlobalFlag = Help
                | Verbose String
                | Sandbox FilePath
                | CabalConf FilePath
                | Version Bool
                  deriving (Eq, Show)

globalOpts :: [OptDescr GlobalFlag]
globalOpts = [ Option "h?" ["help"] (NoArg Help) "Show help text"
             , Option "s" ["sandbox"] (ReqArg Sandbox "DIR")
               "The location of the development cabal sandbox (default: ./cabal-dev)"
             , Option "c" ["config"] (ReqArg CabalConf "PATH")
               "The location of the cabal-install config file (default: use included)"
             , Option "v" ["verbose"] (ReqArg Verbose "LEVEL")
               "Verbosity level: 0 (silent) - 3 (deafening)"
             , Option "" ["version"] (NoArg (Version False))
               "Show the version of this program"
             , Option "" ["numeric-version"] (NoArg (Version True))
               "Show a machine-readable version number"
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
#if MIN_VERSION_Cabal(1,4,0)
getVerbosity flgs =
    case map (runReadE flagToVerbosity) [ s | Verbose s <- flgs ] of
      (Right v:_) -> v
      _           -> normal
#elif MIN_VERSION_Cabal(1,2,0)
getVerbosity flgs = flagToVerbosity $ listToMaybe [ s | Verbose s <- flgs ]
#else
#error Unsupported cabal version
#endif
