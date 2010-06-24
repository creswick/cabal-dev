module Paths_cabal_dev
    ( getDataFileName
    , version
    )
where

import System.FilePath ( (</>) )
import System.Directory ( getCurrentDirectory )

import Data.Version ( Version(..) )

version :: Version
version = Version [9999] ["development"]

getDataFileName :: FilePath -> IO FilePath
getDataFileName p = fmap (</> p) getCurrentDirectory