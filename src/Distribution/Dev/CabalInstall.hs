{-# LANGUAGE TemplateHaskell #-}
module Distribution.Dev.CabalInstall
       ( findOnPath
       , program
       , getFeatures
       , CabalFeatures
       , needsQuotes
       , hasOnlyDependencies
       , configDir
       , CabalCommand(..)
       , LongOption(..)
       , matchOption
       , commandToString
       , stringToCommand
       , allCommands
       , commandOptions
       , supportsOption
       , supportedOptions
       )
where

import Data.List ( tails, isPrefixOf )
import Control.Applicative ( (<$>) )
import Distribution.Version ( Version(..), withinRange
                            , earlierVersion, orLaterVersion )
import Distribution.Verbosity ( Verbosity )
import Distribution.Simple.Program ( Program( programFindVersion
                                            )
                                   , ConfiguredProgram
                                   , emptyProgramConfiguration
                                   , findProgramVersion
                                   , programLocation
                                   , programVersion
                                   , requireProgram
                                   , getProgramOutput
                                   , simpleProgram
                                   )
import Distribution.Simple.Utils ( debug )
import Distribution.Text ( display, simpleParse )

import System.Directory ( getAppUserDataDirectory )

import Distribution.Dev.TH.DeriveCabalCommands
    ( deriveCabalCommands, LongOption(..) )

-- XXX This is duplicated in Setup.hs
-- |Definition of the cabal-install program
program :: Program
program =
    (simpleProgram "cabal") { programFindVersion =
                                  findProgramVersion "--numeric-version" id
                            }

-- |Find cabal-install on the user's PATH
findOnPath :: Verbosity -> IO ConfiguredProgram
findOnPath v = do
  (cabal, _) <- requireProgram v program emptyProgramConfiguration
  debug v $ concat [ "Using cabal-install "
                   , maybe "(unknown version)" display $ programVersion cabal
                   , " at "
                   , show (programLocation cabal)
                   ]
  return cabal

-- |Parse the Cabal library version from the output of cabal --version
parseVersionOutput :: String -> Either String Version
parseVersionOutput str =
    case lines str of
      []      -> Left "No version string provided."
      [_]     -> Left "Could not find Cabal version line."
      (_:ln:_) -> case simpleParse ((words ln)!!2) of
                   Just v  -> Right v
                   Nothing -> Left $ err ln
        where err line = "Could not parse Cabal verison.\n"
                         ++ "(simpleParse "++show line++")"

-- |The information necessary to properly invoke cabal-install
data CabalFeatures = CabalFeatures { cfLibVersion :: Version
                                   , cfExeVersion :: Version
                                   }

mkVer :: [Int] -> Version
mkVer l = Version l []

-- |Extract the features of this cabal-install executable
getFeatures :: Verbosity -> ConfiguredProgram ->
               IO (Either String CabalFeatures)
getFeatures v cabal = do
  case programVersion cabal of
    Nothing -> return $ Left "Failed to find cabal-install version"
    Just exeVer -> do
      verRes <- parseVersionOutput <$> getProgramOutput v cabal ["--version"]
      case verRes of
        Left err -> return $ Left $ "Detecting cabal-install's Cabal: " ++ err
        Right libVer -> return $ Right $
                        CabalFeatures { cfLibVersion = libVer
                                      , cfExeVersion = exeVer
                                      }

-- |Does the cabal-install configuration file use quoted paths in the
-- install-dirs section?
needsQuotes :: CabalFeatures -> Bool
needsQuotes = (`withinRange` earlierVersion (mkVer [1,10])) . cfLibVersion

-- |Does this cabal-install executable support the --dependencies-only
-- flag to install?
hasOnlyDependencies :: CabalFeatures -> Bool
hasOnlyDependencies =
  (`withinRange` orLaterVersion (mkVer [0, 10])) . cfExeVersion

$(deriveCabalCommands)

supportsOption :: CabalCommand -> String -> Bool
supportsOption cc s = any (`matchOption` s) $ supportedOptions cc

supportedOptions :: CabalCommand -> [LongOption]
supportedOptions cc = commonOptions ++ commandOptions cc

matchOption :: LongOption -> String -> Bool
matchOption (LongOption s) = (== s)
matchOption (ProgBefore s) = any (== ('-':s)) . tails
matchOption (ProgAfter s) = ((s ++ "-") `isPrefixOf`)

commonOptions :: [LongOption]
commonOptions = [LongOption "config-file"]

-- |What is the configuration directory for this cabal-install executable?

-- XXX: This needs to do something different for certain platforms for
-- new versions of cabal-install (look at the tickets on creswick's
-- cabal-dev repo)
configDir :: CabalFeatures -> IO FilePath
configDir _ = getAppUserDataDirectory "cabal"
