{- Copyright (c) 2010 Galois, Inc. -}
{-# LANGUAGE CPP #-}
module Distribution.Dev.Flags
    ( GlobalFlag(..)

    , Config
    , getCabalConfig
    , getSandbox
    , sandboxSpecified
    , getVerbosity
    , fromFlags
    , passthroughArgs
    , cfgCabalInstall

    , globalOpts
    , parseGlobalFlags
    , helpRequested
    , getOpt''
    )
where

import Control.Monad          ( mplus )
import Data.Monoid            ( Monoid(..) )
import Data.List              ( intercalate )
import Data.Maybe             ( fromMaybe, isJust, maybeToList, listToMaybe )
import Data.Foldable          ( foldMap )
import System.FilePath        ( (</>) )
import Distribution.ReadE     ( runReadE )
import Distribution.Verbosity ( Verbosity, normal, verbose, flagToVerbosity )
import Paths_cabal_dev        ( getDataFileName )
import System.Console.GetOpt  ( OptDescr(..), ArgOrder(..), ArgDescr(..)
                              , getOpt', getOpt
                              )

import qualified Distribution.Dev.CabalInstall as CI

data GlobalFlag = Help
                | Verbose (Maybe String)
                | Sandbox FilePath
                | CabalConf FilePath
                | Version Bool
                | CabalInstallArg String
                | WithCabalInstall FilePath
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
             , Option "" ["cabal-install-arg"] (ReqArg CabalInstallArg "ARG") $
               "Pass this argument through to cabal-install untouched (in " ++
               "case an argument to cabal-install conflicts with an " ++
               "argument to cabal-dev"
             , Option "" ["with-cabal-install"] (ReqArg WithCabalInstall "PATH") $
               "The location of the specific cabal-install to invoke " ++
               "(defaults to looking on your PATH)"
             ]

cabalArgToOptDescr :: CI.Option -> OptDescr GlobalFlag
cabalArgToOptDescr (CI.Option cn ty) =
    Option shortName longName parse "<internal implementation>"
    where
      shortName = case cn of
                    CI.Short c -> [c]
                    _          -> []

      longName  = case cn of
                    CI.LongOption s -> [s]
                    _               -> []

      optName = case cn of
                  CI.Short c      -> ['-',c]
                  CI.LongOption s -> '-':'-':s

      noArg = NoArg $ CabalInstallArg optName
      withArg s = case cn of
                    CI.Short _      -> optName ++ s
                    CI.LongOption _ -> optName ++ '=':s
      parse =
          case ty of
            CI.NoArg -> noArg
            CI.Req -> ReqArg (CabalInstallArg . withArg) "INTERNAL"
            CI.Opt -> OptArg (CabalInstallArg . maybe optName withArg) "INTERNAL"

getOpt'' :: [OptDescr a] -> [String] -> ([a], [String], [String])
getOpt'' opts args =
    case break (== "--") args of
      (_, []) ->
          -- Mixed args (make a best guess about which ones are ours
          -- and attempt to preserve the others. This gets especially
          -- confused for unknown short options that take arguments.
          -- (e.g. cabal install -ftest)
          let (flgs, args', unknown, errs) = getOpt' Permute opts args
              unusedArgs = args' ++ unknown
              -- Attempt to get the arguments back into the order that they
              -- were passed in, so for example, if there is an argument
              -- --foo -o xxx -bar, we keep the xxx as a potential argument
              -- to -o
              unprocessed = [arg | arg <- args, arg `elem` unusedArgs]
          in (flgs, unprocessed, errs)

      (ourArgs, "--":theirArgs) ->
          let (flgs, extraArgs, errs) = getOpt RequireOrder opts ourArgs
              errs' | null extraArgs = errs
                    | otherwise      =
                        let msg = "Unknown arguments for cabal-dev: " ++
                                  intercalate " " extraArgs
                        in (msg:errs)
          in (flgs, theirArgs, errs')

      impossible -> error $
                    "Impossible outcome from break: " ++ show impossible

parseGlobalFlags :: [String] -> ([GlobalFlag], [String], [String])
parseGlobalFlags args = getOpt'' (globalOpts ++ defs) args
    where
      cmd = CI.stringToCommand =<< listToMaybe (dropWhile isOpt args)
      defs = map cabalArgToOptDescr $ CI.commandOptions =<< maybeToList cmd
      isOpt = (== "-") . take 1

helpRequested :: [GlobalFlag] -> Bool
helpRequested = (Help `elem`)

getCabalConfig :: Config -> IO FilePath
getCabalConfig = maybe defaultFileName return . cfgCabalConfig
    where
      defaultFileName = getDataFileName $ "admin" </> "cabal-config.in"

getVerbosity :: Config -> Verbosity
getVerbosity = fromMaybe normal . cfgVerbosity

defaultSandbox :: FilePath
defaultSandbox = "cabal-dev"

getSandbox :: Config -> FilePath
getSandbox = fromMaybe defaultSandbox . cfgSandbox

sandboxSpecified :: Config -> Bool
sandboxSpecified = isJust . cfgSandbox

data Config = Config { cfgVerbosity   :: Maybe Verbosity
                     , cfgCabalConfig :: Maybe FilePath
                     , cfgSandbox     :: Maybe FilePath
                     , cfgCabalInstall :: Maybe FilePath
                     , passthroughArgs :: [String]
                     }

instance Monoid Config where
    mempty = Config Nothing Nothing Nothing Nothing []
    mappend (Config v1 c1 s1 ci1 a1) (Config v2 c2 s2 ci2 a2) =
        Config (v2 `mplus` v1) (c2 `mplus` c1) (s2 `mplus` s1) (ci1 `mplus` ci2) (a1 ++ a2)

fromFlag :: GlobalFlag -> Config
fromFlag (CabalConf p) = mempty { cfgCabalConfig = Just p }
fromFlag (Sandbox s)   = mempty { cfgSandbox = Just s }
fromFlag (WithCabalInstall p) = mempty { cfgCabalInstall = Just p }
fromFlag (Verbose s)   = mempty { cfgVerbosity = v }
    where
      v = case runReadE flagToVerbosity `fmap` s of
            Nothing        -> Just verbose
            Just (Right x) -> Just x
            Just _         -> Nothing -- XXX: we are ignoring
                                      -- verbosity parse errors
fromFlag (CabalInstallArg a) = mempty { passthroughArgs = [a] }
fromFlag _             = mempty

fromFlags :: [GlobalFlag] -> Config
fromFlags = foldMap fromFlag
