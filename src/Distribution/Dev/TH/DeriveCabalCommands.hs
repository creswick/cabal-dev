module Distribution.Dev.TH.DeriveCabalCommands
    ( getCabalCommands
    , deriveCabalCommands
    , mkCabalCommandsDef
    )
where

import Control.Applicative ( (<$>) )
import Data.Char ( toUpper, isSpace )
import Data.List ( isPrefixOf )
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
         [ Clause [] (NormalB (ListE $ map ccE cmds)) []
         ]
       ]

ccE :: CabalCommandStr -> Exp
ccE = LitE . StringL . ccStr

mkCmdToStr :: [CabalCommandStr] -> [Dec]
mkCmdToStr =
    fromCommandClauses "commandToString" (ccT ~~> strT) $ map $ \n ->
    Clause [ConP (commandConsName n) []] (NormalB (ccE n)) []

mkStrToCmd :: [CabalCommandStr] -> [Dec]
mkStrToCmd =
    fromCommandClauses "stringToCommand" (strT ~~> maybeT ccT) $ \ccs ->
        map toClause ccs ++ [nothing]
    where
      toClause n =
          Clause [LitP (StringL (ccStr n))] (NormalB (justE $ ccE n)) []
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
