{-# LANGUAGE CPP #-}
module Distribution.Dev.InvokeCabal
    ( actions
    , setup
    , extraArgs
    , cabalProgram
    , cabalArgs
    )
where

import Distribution.Version ( Version(..) )
import Distribution.Verbosity ( Verbosity, showForCabal )
import Distribution.Simple.Program ( Program( programFindVersion
                                            , programFindLocation
                                            )
                                   , emptyProgramConfiguration
                                   , findProgramVersion
                                   , locationPath
                                   , programLocation
                                   , programVersion
                                   , requireProgram
                                   , runProgram
                                   , simpleProgram
                                   )
import Distribution.Simple.Utils ( withUTF8FileContents, writeUTF8File
                                 , debug, cabalVersion )
import Distribution.Text ( display, simpleParse )
import System.Console.GetOpt  ( OptDescr )

import Distribution.Dev.Command            ( CommandActions(..)
                                           , CommandResult(..)
                                           )
import Distribution.Dev.Flags              ( Config, getCabalConfig
                                           , getVerbosity
                                           )
import Distribution.Dev.InitPkgDb          ( initPkgDb )
import qualified Distribution.Dev.RewriteCabalConfig as R
import Distribution.Dev.Sandbox            ( resolveSandbox
                                           , cabalConf
                                           , Sandbox
                                           , KnownVersion
                                           , PackageDbType(..)
                                           , getVersion
                                           , pkgConf
                                           , sandbox
                                           )
import Distribution.Dev.Utilities          ( ensureAbsolute )

import System.Directory ( getAppUserDataDirectory )

actions :: String -> CommandActions
actions act = CommandActions
              { cmdDesc = "Invoke cabal-install with the development configuration"
              , cmdRun = \flgs _ args -> invokeCabal flgs (act:args)
              , cmdOpts = [] :: [OptDescr ()]
              , cmdPassFlags = True
              }

invokeCabal :: Config -> [String] -> IO CommandResult
invokeCabal flgs args = do
  let v = getVerbosity flgs
  s <- initPkgDb v =<< resolveSandbox flgs
  res <- setup s flgs
  case res of
    Left err -> return $ CommandError err
    Right args' -> do
             invokeCabalCfg v $ args' ++ args
             return CommandOk

cabalArgs :: Config -> IO (Either String [String])
cabalArgs flgs = do
  let v = getVerbosity flgs
  s <- initPkgDb v =<< resolveSandbox flgs
  setup s flgs

setup :: Sandbox KnownVersion-> Config -> IO (Either String [String])
setup s flgs = do
  let v = getVerbosity flgs
  cfgIn <- getCabalConfig flgs
  let cfgOut = cabalConf s
  cabalHome <- getAppUserDataDirectory "cabal"
  withUTF8FileContents cfgIn $ \cIn ->
      do cfgRes <- R.rewriteCabalConfig (R.Rewrite cabalHome (sandbox s) (pkgConf s)) cIn
         case cfgRes of
           Left err -> return $ Left $
                       "Error processing cabal config file " ++ cfgIn ++ ": " ++ err
           Right cOut -> do
                       writeUTF8File cfgOut cOut
                       args <- extraArgs v cfgOut (getVersion s)
                       return $ Right args

extraArgs :: Verbosity -> FilePath -> PackageDbType -> IO [String]
extraArgs v cfg pdb =
    do pdbArgs <- getPdbArgs
       return $ [cfgFileArg, verbosityArg] ++ pdbArgs
    where
      longArg s = showString "--" . showString s . ('=':)
      cfgFileArg = longArg "config-file" cfg
      verbosityArg = longArg "verbose" $ showForCabal v
      withGhcPkg = longArg "with-ghc-pkg"
      getPdbArgs =
          case pdb of
            (GHC_6_8_Db loc) | needsGHC68Compat -> do
                     -- Make Cabal call the wrapper that removes the
                     -- bad argument to ghc-pkg 6.8
                     debug v $ "Using GHC 6.8 compatibility wrapper for Cabal shortcoming"
                     (ghcPkgCompat, _) <-
                         requireProgram v ghcPkgCompatProgram emptyProgramConfiguration
                     return $ [ longArg "ghc-pkg-options" $ withGhcPkg loc
                              , withGhcPkg $ locationPath $
                                programLocation ghcPkgCompat
                              ]
            _ -> return []

-- XXX: this is very imprecise. Right now, we require a specific
-- version of Cabal, so this is ok (and is equivalent to True)
needsGHC68Compat :: Bool
needsGHC68Compat = cabalVersion < Version [1, 9] []

ghcPkgCompatProgram :: Program
ghcPkgCompatProgram  = p { programFindLocation =
                           \v -> do
                             res <- programFindLocation p v
                             case res of
                               Nothing -> return Nothing
                               Just loc -> Just `fmap` ensureAbsolute loc
                         }
    where
      p = simpleProgram "ghc-pkg-6_8-compat"

-- XXX This is duplicated in Setup.hs
cabalProgram :: Program
cabalProgram =
    (simpleProgram "cabal") { programFindVersion =
                                  findProgramVersion "--numeric-version" id
                            }

invokeCabalCfg :: Verbosity -> [String] -> IO ()
invokeCabalCfg v args = do
  (cabal, _) <- requireProgram v cabalProgram emptyProgramConfiguration
  debug v $ concat [ "Using cabal-install "
                   , maybe "(unknown version)" display $ programVersion cabal
                   , " at "
                   , show (programLocation cabal)
                   ]
  runProgram v cabal args

cabalVersion :: String -> Either String Version
cabalVersion str =
    case lines str of
      []      -> Left "No version string provided."
      [x]     -> Left "Could not find Cabal version line."
      (_:ln:_) -> case simpleParse ((words ln)!!2) of
                   Just v  -> Right v
                   Nothing -> Left $ err ln
        where err ln = "Could not parse Cabal verison.\n"
                       ++ "(simpleParse "++show ln++")"