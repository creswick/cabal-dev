{-|

Support for determining the interface of cabal-install via its --help
output.

 -}
module Distribution.Dev.InterrogateCabalInstall
    ( CabalCommandStr
    , ccStr
    , parseCabalHelp
    , Option(..)
    , OptionName(..)
    , ArgType(..)
    , optParseFlags
    , getCabalCommandHelp
    , getCabalHelp
    , getCabalCommands
    , Program
    , progStr
    , getCabalProgs
    )
where

import Control.Applicative ( (<$>) )
import Data.Char ( isSpace, isAsciiUpper, isAsciiLower, ord )
import Data.List ( isPrefixOf, sort )
import Control.Monad ( guard )
import Data.Maybe ( mapMaybe )
import Distribution.Simple.Utils ( rawSystemStdout )
import Distribution.Verbosity ( verbose )

-- |A cabal-install command name
newtype CabalCommandStr = CabalCommandStr { ccStr :: String }

newtype Program = Program { progStr :: String } deriving (Show, Eq)

-- |Get the command names from a String containing the output of
-- cabal-install --help
parseCabalHelp :: String -> [CabalCommandStr]
parseCabalHelp = map (CabalCommandStr . extractName) .
                 takeCommands .
                 dropTillCommands .
                 lines
    where
      extractName = takeWhile (not . isSpace) . dropWhile isSpace
      takeCommands = takeWhile (not . all isSpace)
      dropTillCommands = drop 1 .
                         dropWhile (not . ("Commands:" `isPrefixOf`))

-- |The kinds of options supported by cabal-install
--
-- XXX: this should also record whether an argument is required.
--
-- XXX: this should also parse short options
data OptionName
    = LongOption String
    | Short      Char
      deriving (Eq, Show)

progBeforeOpt :: [Program] -> String -> [OptionName]
progBeforeOpt progs s = map (\p -> LongOption $ progStr p ++ '-':s) progs

progAfterOpt :: [Program] -> String -> [OptionName]
progAfterOpt progs s = map (LongOption . (s ++) . ('-':) . progStr) progs

data Option = Option OptionName ArgType deriving (Show, Eq)

data ArgType = Req | Opt | NoArg deriving (Eq, Show)

parseProgs :: String -> [Program]
parseProgs = map Program . words . concat . takeWhile (not . all isSpace) .
             drop 1 . dropWhile (not . isProgIntro) . lines
    where
      isProgIntro s = (reverse "can be used with the following programs:")
                      `isPrefixOf` (dropWhile isSpace $ reverse s)

-- |Parse the output of 'cabal foo --help' to determine the valid
-- options for 'cabal foo'
--
-- Note that the --config-file flag is never documented.
optParseFlags :: [Program] -> String -> [Option]
optParseFlags progs = extractLongOptions . findOptionLines . lines
    where
      findOptionLines = takeWhile (not . all isSpace) .
                        drop 1 .
                        dropWhile (not . ("Flags for " `isPrefixOf`))

      leftmostDoubleDash = take 1 . sort . mapMaybe (findDoubleDash 0)

      extractLongOptions ls = do
        i <- leftmostDoubleDash ls
        guard $ checkLoc i ls
        (soptStr, (' ':l@('-':'-':_))) <- splitAt i <$> ls
        let (doubleOpts, tys) = unzip $ parseDoubleOpts l
            ty = case tys of
                   (t:_) -> t
                   []    -> NoArg
        map (\n -> Option n ty) $ parseSingleOpts soptStr ++ doubleOpts

      -- Check that the spot that we picked to split is either the
      -- start of a long option description or whitespace
      checkLoc i = all (`elem` [" --", "   "]) . map (take 3 . drop i)

      findDoubleDash n (' ':'-':'-':_) = Just n
      findDoubleDash _ []              = Nothing
      findDoubleDash n (_:xs)          = let n' = n + 1
                                         in n' `seq` findDoubleDash n' xs

      parseDoubleOpts ('-':'-':xs) = do
        (optName, rest) <- plainOpt xs ++ progBefore xs ++ progAfter xs
        let (eoc, ty) =
                case take 2 rest of
                  ['=',_] -> (dropWhile isAsciiUpper $ drop 1 rest, Req)
                  "[="    -> (drop 1 $ dropWhile isAsciiUpper $ drop 2 rest, Opt)
                  _       -> (rest, NoArg)
            opt = (optName, ty)
        case eoc of
          (',':' ':rest') -> opt:parseDoubleOpts rest'
          (' ':_)         -> [opt]
          []              -> [opt]
          _               -> []

      parseDoubleOpts _ = []

      parseSingleOpts s =
          case dropWhile isSpace s of
            ('-':c:rest)
                | isAsciiLower c || isAsciiUpper c ->
                    case rest of
                      (',':' ':s') -> Short c:parseSingleOpts s'
                      []           -> [Short c]
                      (' ':_)      -> [Short c]
                      _            -> []
            _                                      -> []

      optChar c = ord c < 128 && (isAsciiLower c || c == '-')

      plainOpt s = let (c, rest) = break (not . optChar) s
                   in do guard $ not $ null c
                         return (LongOption c, rest)

      progBefore s = case break (== '-') s of
                       ("PROG", '-':rest) ->
                           do (LongOption n, rest') <- plainOpt rest
                              o <- progBeforeOpt progs n
                              return (o, rest')
                       _ -> []

      progAfter s = do (LongOption n, rest) <- plainOpt s
                       guard $ take 1 (reverse n) == "-"
                       case break (not . isAsciiUpper) rest of
                         ("PROG", rest') -> do o <- progAfterOpt progs $ init n
                                               return (o, rest')
                         _               -> []

-- |Obtain the --help output for a particular cabal-install command
getCabalCommandHelp :: CabalCommandStr -> IO String
getCabalCommandHelp c = rawSystemStdout verbose "cabal" [ccStr c, "--help"]

-- |Obtain the top-level --help output for cabal-install
getCabalHelp :: IO String
getCabalHelp = rawSystemStdout verbose "cabal" ["--help"]

-- |Invoke cabal-install in order to determine what commands it
-- supports.
getCabalCommands :: IO [CabalCommandStr]
getCabalCommands = parseCabalHelp <$> getCabalHelp

getCabalProgs :: IO [Program]
getCabalProgs = parseProgs <$> getCabalCommandHelp (CabalCommandStr "install")