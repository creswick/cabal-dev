module Distribution.Dev.TH.DeriveCabalCommands
    ( getCabalCommands
    , deriveCabalCommands
    , mkCabalCommandsDef
    , LongOption(..)
    , optParseFlags
    )
where

import Control.Applicative ( (<$>) )
import Data.Char ( toUpper, isSpace, isAsciiUpper, isAsciiLower, ord )
import Data.List ( isPrefixOf, sort )
import Control.Monad ( guard )
import Data.Maybe ( mapMaybe )
import Language.Haskell.TH
import Distribution.Simple.Utils ( rawSystemStdout )
import Distribution.Verbosity ( verbose )

newtype CabalCommandStr = CabalCommandStr { ccStr :: String }

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

data LongOption
    = LongOption String
    | ProgBefore String
    | ProgAfter  String
      deriving (Eq, Show)

optParseFlags :: String -> [LongOption]
optParseFlags = extractLongOptions . findOptionLines . lines
    where
      findOptionLines = takeWhile (not . all isSpace) .
                        drop 1 .
                        dropWhile (not . ("Flags for " `isPrefixOf`))

      leftmostDoubleDash = take 1 . sort . mapMaybe (findDoubleDash 0)

      extractLongOptions ls = do
        i <- leftmostDoubleDash ls
        guard $ checkLoc i ls
        l@('-':'-':_) <- drop (i + 1) <$> ls
        parseDoubleOpts l

      checkLoc i = all (`elem` [" --", "   "]) . map (take 3 . drop i)

      findDoubleDash n (' ':'-':'-':_) = Just n
      findDoubleDash _ []              = Nothing
      findDoubleDash n (_:xs)          = let n' = n + 1
                                         in n' `seq` findDoubleDash n' xs
      parseDoubleOpts ('-':'-':xs) = do
        (optName, rest) <- plainOpt xs ++ progBefore xs ++ progAfter xs
        let eoc = case take 2 rest of
                    ['=',_] -> dropWhile isAsciiUpper $ drop 1 rest
                    "[=" -> drop 1 $ dropWhile isAsciiUpper $ drop 2 rest
                    _   -> rest
        case eoc of
          (',':' ':rest') -> optName:parseDoubleOpts rest'
          (' ':_)         -> [optName]
          []              -> [optName]
          _               -> []

      parseDoubleOpts _ = []

      optChar c = ord c < 128 && (isAsciiLower c || c == '-')

      plainOpt s = let (c, rest) = break (not . optChar) s
                   in do guard $ not $ null c
                         return (LongOption c, rest)

      progBefore s = case break (== '-') s of
                       ("PROG", '-':rest) ->
                           do (LongOption n, rest') <- plainOpt rest
                              return (ProgBefore n, rest')
                       _ -> []
      progAfter s = do (LongOption n, rest) <- plainOpt s
                       guard $ take 1 (reverse n) == "-"
                       case break (not . isAsciiUpper) rest of
                         ("PROG", rest') -> return (ProgAfter $ init n, rest')
                         _               -> []

mkLO :: LongOption -> Exp
mkLO lo = let (cn, o) = case lo of
                          LongOption s -> ("LongOption", s)
                          ProgBefore s -> ("ProgBefore", s)
                          ProgAfter  s -> ("ProgAfter",  s)
          in AppE (ConE (mkName cn)) $ LitE $ StringL o

mkSupportedOptionClause :: CabalCommandStr -> String -> Clause
mkSupportedOptionClause cStr helpOutput =
    let supportedFlags = ListE . map mkLO .
                         optParseFlags $ helpOutput
    in Clause [ConP (commandConsName cStr) []] (NormalB supportedFlags) []

getCabalCommandHelp :: CabalCommandStr -> IO String
getCabalCommandHelp c = rawSystemStdout verbose "cabal" [ccStr c, "--help"]

mkGetSupportedOptions :: [(CabalCommandStr, String)] -> [Dec]
mkGetSupportedOptions cs =
    let n = mkName "commandOptions"
    in [ SigD n $ ccT ~~> AppT ListT (ConT (mkName "LongOption"))
       , FunD n $ map (uncurry mkSupportedOptionClause) cs
       ]

mkGetSupportedOptionsIO :: [CabalCommandStr] -> IO [Dec]
mkGetSupportedOptionsIO ccs =
    mkGetSupportedOptions . zip ccs <$> mapM getCabalCommandHelp ccs

getCabalHelp :: IO String
getCabalHelp = rawSystemStdout verbose "cabal" ["--help"]

getCabalCommands :: IO [CabalCommandStr]
getCabalCommands = parseCabalHelp <$> getCabalHelp

mkCabalCommandsDef :: [CabalCommandStr] -> IO [Dec]
mkCabalCommandsDef strs =
    do putStrLn "Interrogating cabal-install executable:"
       concat <$> mapM ($ strs)
                  [ return . return . cabalCommandsDef
                  , return . mkStrToCmd
                  , return . mkCmdToStr
                  , return . mkAllCommands
                  , mkGetSupportedOptionsIO
                  ]

deriveCabalCommands :: Q [Dec]
deriveCabalCommands = runIO $ mkCabalCommandsDef =<< getCabalCommands

mkAllCommands :: [CabalCommandStr] -> [Dec]
mkAllCommands cmds =
    let n = mkName "allCommands"
    in [ SigD n $ AppT ListT ccT
       , FunD n
         [ Clause [] (NormalB (ListE $ map (ConE . commandConsName) cmds)) []
         ]
       ]

ccL :: CabalCommandStr -> Lit
ccL = StringL . ccStr

mkCmdToStr :: [CabalCommandStr] -> [Dec]
mkCmdToStr =
    fromCommandClauses "commandToString" (ccT ~~> strT) $ map $ \n ->
    Clause [ConP (commandConsName n) []] (NormalB (LitE $ ccL n)) []

mkStrToCmd :: [CabalCommandStr] -> [Dec]
mkStrToCmd =
    fromCommandClauses "stringToCommand" (strT ~~> maybeT ccT) $ \ccs ->
        map toClause ccs ++ [nothing]
    where
      toClause n =
          Clause [LitP (ccL n)] (NormalB (justE $ ccE n)) []
      justE = AppE $ ConE $ mkName "Just"
      ccE = ConE . commandConsName
      nothing = Clause [WildP] (NormalB (ConE (mkName "Nothing"))) []

cabalCommandsDef :: [CabalCommandStr] -> Dec
cabalCommandsDef strs = DataD [] ccN [] (map cStrToCon strs) [mkName "Eq"]

ccN :: Name
ccN = mkName "CabalCommand"

commandConsName :: CabalCommandStr -> Name
commandConsName (CabalCommandStr s) = mkName $ capitalize s
    where
      capitalize (c:cs) = toUpper c : cs
      capitalize [] = []

cStrToCon :: CabalCommandStr -> Con
cStrToCon c = NormalC (commandConsName c) []

fromCommandClauses :: String -> Type -> ([CabalCommandStr] -> [Clause])
                   -> [CabalCommandStr] -> [Dec]
fromCommandClauses funName funTy mkClause cmdStrs =
    [ SigD n funTy, FunD n $ mkClause cmdStrs ]
    where
      n = mkName funName

(~~>) :: Type -> Type -> Type
t1 ~~> t2 = AppT (AppT ArrowT t1) t2

strT :: Type
strT = ConT $ mkName "String"

ccT :: Type
ccT = ConT ccN

maybeT :: Type -> Type
maybeT = AppT (ConT $ mkName "Maybe")
