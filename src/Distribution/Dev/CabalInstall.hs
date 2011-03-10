module Distribution.Dev.CabalInstall
       ( findOnPath
       , program
       , getFeatures
       , CabalFeatures
       , needsQuotes
       , hasOnlyDependencies
       , configDir
       )
where

import Control.Arrow ( right )
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

-- XXX This is duplicated in Setup.hs
program :: Program
program =
    (simpleProgram "cabal") { programFindVersion =
                                  findProgramVersion "--numeric-version" id
                            }

findOnPath :: Verbosity -> IO ConfiguredProgram
findOnPath v = do
  (cabal, _) <- requireProgram v program emptyProgramConfiguration
  debug v $ concat [ "Using cabal-install "
                   , maybe "(unknown version)" display $ programVersion cabal
                   , " at "
                   , show (programLocation cabal)
                   ]
  return cabal

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

newtype CabalFeatures = CabalFeatures { cfVersion :: Version }

mkVer :: [Int] -> Version
mkVer l = Version l []

getFeatures :: Verbosity -> ConfiguredProgram ->
               IO (Either String CabalFeatures)
getFeatures v cabal =
  right CabalFeatures . parseVersionOutput <$>
  getProgramOutput v cabal ["--version"]

needsQuotes :: CabalFeatures -> Bool
needsQuotes = (`withinRange` earlierVersion (mkVer [1,10])) . cfVersion

hasOnlyDependencies :: CabalFeatures -> Bool
hasOnlyDependencies =
  (`withinRange` orLaterVersion (mkVer [1, 10])) . cfVersion

-- XXX: This needs to do something different for certain platforms for
-- new versions of cabal-install (look at the tickets on creswick's
-- cabal-dev repo)
configDir :: CabalFeatures -> IO FilePath
configDir _ = getAppUserDataDirectory "cabal"
