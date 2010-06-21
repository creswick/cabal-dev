{- Copyright (c) 2010 Galois, Inc. -}
{-|

Message logging

-}
module Distribution.Dev.Log
    ( Level(..)
    , logMsg
    , requestedLevel

    , debug
    , normal
    )
where

import Distribution.Dev.Flags ( GlobalFlag(Verbose) )

data Level = Normal | Debug deriving (Eq, Ord)

requestedLevel :: [GlobalFlag] -> Level
requestedLevel flgs =
    case [ () | Verbose <- flgs ] of
      [] -> Normal
      _  -> Debug

logMsg :: Level -> [GlobalFlag] -> String -> IO ()
logMsg lvl flgs | shouldLog = putStrLn
                | otherwise = const $ return ()
    where
      shouldLog = requestedLevel flgs >= lvl

debug :: [GlobalFlag] -> String -> IO ()
debug = logMsg Debug

normal :: [GlobalFlag] -> String -> IO ()
normal = logMsg Normal