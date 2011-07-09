module Distribution.Dev.TH.DeriveCabalCommands
    ( getCabalCommands
    , deriveCabalCommands
    , mkCabalCommandsDef
    , LongOption(..)
    , optParseFlags
    )
where

import Control.Applicative ( (<$>) )
import Data.Char ( toUpper, isSpace, isAsciiLower, ord )
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

newtype LongOption = LongOption { longOptStr :: String }

optParseFlags :: String -> [LongOption]
optParseFlags = map LongOption . extractLongOptions . findOptionLines . lines
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
      parseDoubleOpts ('-':'-':xs) =
          let (optName, rest) = break (not . optChar) xs
              eoc = case take 1 rest of
                      "=" -> dropWhile (not . (`elem` ", ")) rest
                      _   -> rest
          in case eoc of
               (',':' ':rest') -> optName:parseDoubleOpts rest'
               (' ':_)         -> [optName]
               []              -> [optName]
               _               -> []
      parseDoubleOpts _ = []
      optChar c = ord c < 128 && (isAsciiLower c || c == '-')

getCabalHelp :: IO String
getCabalHelp = rawSystemStdout verbose "cabal" ["--help"]

getCabalCommands :: IO [CabalCommandStr]
getCabalCommands = parseCabalHelp <$> getCabalHelp

mkCabalCommandsDef :: [CabalCommandStr] -> [Dec]
mkCabalCommandsDef strs =
    concat $ map ($ strs) [return . cabalCommandsDef, mkStrToCmd, mkCmdToStr, mkAllCommands]

deriveCabalCommands :: Q [Dec]
deriveCabalCommands = mkCabalCommandsDef <$> runIO getCabalCommands

mkAllCommands :: [CabalCommandStr] -> [Dec]
mkAllCommands cmds =
    let n = mkName "allCommands"
    in [ SigD n $ AppT ListT strT
       , FunD n
         [ Clause [] (NormalB (ListE $ map (LitE . ccL) cmds)) []
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
