{- Copyright (c) 2010 Galois, Inc. -}
{-# LANGUAGE CPP #-}
module Distribution.Dev.Flags
    ( GlobalFlag(..)
    , globalOpts
    , parseGlobalFlags
    , helpRequested
    , getCabalConfig
    , getVerbosity
    , getOpt''
    )
where

import Data.Maybe             ( listToMaybe )
import System.FilePath        ( (</>) )
import Distribution.ReadE     ( runReadE )
import Distribution.Verbosity ( Verbosity, normal, flagToVerbosity )
import Paths_cabal_dev        ( getDataFileName )
import System.Console.GetOpt  ( OptDescr(..), ArgOrder(..), ArgDescr(..)
                              , getOpt'
                              )

data GlobalFlag = Help
                | Verbose (Maybe String)
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
             , Option "v" ["verbose"] (OptArg Verbose "LEVEL")
               "Verbosity level: 0 (silent) - 3 (deafening)"
             , Option "" ["version"] (NoArg (Version False))
               "Show the version of this program"
             , Option "" ["numeric-version"] (NoArg (Version True))
               "Show a machine-readable version number"
             ]

getOpt'' :: [OptDescr a] -> [String] -> ([a], [String], [String])
getOpt'' opts args =
    let (flgs, args', unknown, errs) = getOpt' Permute opts args
        unusedArgs = args' ++ unknown
        -- Attempt to get the arguments back into the order that they
        -- were passed in, so for example, if there is an argument
        -- --foo -o xxx -bar, we keep the xxx as a potential argument
        -- to -o
        unprocessed = [arg | arg <- args, arg `elem` unusedArgs]
    in (flgs, unprocessed, errs)

parseGlobalFlags :: [String] -> ([GlobalFlag], [String], [String])
parseGlobalFlags = getOpt'' globalOpts

helpRequested :: [GlobalFlag] -> Bool
helpRequested = (Help `elem`)

cabalConfigFlag :: [GlobalFlag] -> Maybe FilePath
cabalConfigFlag flgs = listToMaybe [p | CabalConf p <- flgs]

getCabalConfig :: [GlobalFlag] -> IO FilePath
getCabalConfig = maybe defaultFileName return . cabalConfigFlag
    where
      defaultFileName = getDataFileName $ "admin" </> "cabal-config.in"

getVerbosity :: [GlobalFlag] -> Verbosity
getVerbosity flgs =
    case map (fmap $ runReadE flagToVerbosity) [ s | Verbose s <- flgs ] of
      (Just (Right v):_) -> v
      _                  -> normal
