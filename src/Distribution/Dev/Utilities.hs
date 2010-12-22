module Distribution.Dev.Utilities
    ( makeAbsolute
    , makeAbsoluteToCurrentDirectory
    , ensureAbsolute
    ) where

import System.Directory ( getCurrentDirectory )
import System.FilePath ( (</>), isAbsolute, isRelative )

-- | Given a path, returns that path unmodified if it is absolute,
-- otherwise it is made absolute relative to the current directory.
ensureAbsolute :: FilePath -> IO FilePath
ensureAbsolute path | isAbsolute path = return path
                    | otherwise       = makeAbsoluteToCurrentDirectory path

-- | Makes a relative path into an absolute path, rooted in the
-- current directory.  Throws an error if path is not relative.
makeAbsoluteToCurrentDirectory :: FilePath -> IO FilePath
makeAbsoluteToCurrentDirectory path = do cwd <- getCurrentDirectory
                                         case makeAbsolute cwd path of
                                           Left  m -> error m
                                           Right p -> return p

-- | Creates an absolute file path from an absolute path and a
-- relative path.  Does not normalize the result. makeAbsolute returns
-- Left msg if root is not absolute, or rel is not relative
makeAbsolute :: FilePath -> FilePath -> Either String FilePath
makeAbsolute root rel | not (isAbsolute root) = Left (root ++ " was not an absolute path.")
                      | not (isRelative rel)  = Left (rel ++ " was not a relative path.")
                      | otherwise             = Right (root </> rel)

