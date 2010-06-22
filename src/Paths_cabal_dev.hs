module Paths_cabal_dev
    ( getDataFileName
    )
where

import System.FilePath ( (</>) )
import System.Directory ( getCurrentDirectory )

getDataFileName :: FilePath -> IO FilePath
getDataFileName p = fmap (</> p) getCurrentDirectory