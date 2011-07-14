{-|

Mark up and extract command-line arguments in a simple text format.

>>> formatGHCArgs ["foo"] == unlines [marker start, "foo", marker end]
True
>>> let check args = [args] == extractGHCArgs (formatGHCArgs args)
>>> check ["foo"]
True
>>> check ["-Wall", "-Werror"]
True

-}
module Distribution.Dev.GhcArgs
    ( formatGHCArgs
    , extractGHCArgs
    )
where

import Data.List ( unfoldr )

-- |Format the arguments, bracketed by start end end markers, in a way
-- that can be easily parsed.
--
-- Each argument is on its own line to avoid having to escape/parse
-- strings. This assumes that there are no newlines in the arguments.
formatGHCArgs :: [String] -> String
formatGHCArgs = unlines . (++ [marker end]) . (marker start:)

-- |Extract the output from formatGHCArgs out of a String containing
-- program output.
--
-- If there were multiple invocations of GHC, then return multiple
-- sets of arguments.
extractGHCArgs :: String -> [[String]]
extractGHCArgs = unfoldr step . lines
    where
      step ls =
          case findMarkers ls of
            (args, (_endMarker:rest)) -> Just (args, rest)
            (_,    [])                -> Nothing -- No end marker was found

      findMarkers =
          break (isMarker end) . drop 1 . dropWhile (not . isMarker start)

-- |Format a start or end marker
--
-- This is used to extract GHC arguments out of the output from
-- cabal-install, where there may be multiple invocations of GHC as
-- well as much other output.
marker :: Marker -> String
marker (Marker s) = concat ["== GHC Arguments: ", s, " =="]

isMarker :: Marker -> String -> Bool
isMarker m = (== marker m)

end :: Marker
end = Marker "End"

start :: Marker
start = Marker "Start"

newtype Marker = Marker String
