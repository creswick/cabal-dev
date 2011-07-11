module Distribution.Dev.TH.DeriveCabalCommands
    ( getCabalCommands
    , deriveCabalCommands
    , mkCabalCommandsDef
    , optParseFlags
    )
where

import Control.Applicative ( (<$>) )
import Data.Char ( toUpper  )
import Language.Haskell.TH
import Distribution.Dev.InterrogateCabalInstall
    ( Option(..), OptionName(..), ArgType(..), CabalCommandStr, ccStr
    , optParseFlags, getCabalCommandHelp, getCabalCommands )

mkLO :: Option -> Exp
mkLO (Option on ty) =
    let (cn, o) = case on of
                    Short c -> ("Short", CharL c)
                    LongOption s -> ("LongOption", StringL s)
                    ProgBefore s -> ("ProgBefore", StringL s)
                    ProgAfter  s -> ("ProgAfter",  StringL s)

        tyE = ConE $ mkName $
              case ty of
                Req -> "Req"
                Opt -> "Opt"
                NoArg -> "NoArg"

        nE = appC cn $ LitE o

        optC n t = AppE (appC "Option" n) t

        appC n = AppE $ ConE $ mkName n

    in optC nE tyE

mkSupportedOptionClause :: CabalCommandStr -> String -> Clause
mkSupportedOptionClause cStr helpOutput =
    let supportedFlags = ListE . map mkLO .
                         optParseFlags $ helpOutput
    in Clause [ConP (commandConsName cStr) []] (NormalB supportedFlags) []

mkGetSupportedOptions :: [(CabalCommandStr, String)] -> [Dec]
mkGetSupportedOptions cs =
    let n = mkName "commandOptions"
    in [ SigD n $ ccT ~~> AppT ListT (ConT (mkName "Option"))
       , FunD n $ map (uncurry mkSupportedOptionClause) cs
       ]

mkGetSupportedOptionsIO :: [CabalCommandStr] -> IO [Dec]
mkGetSupportedOptionsIO ccs =
    mkGetSupportedOptions . zip ccs <$> mapM getCabalCommandHelp ccs

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
commandConsName = mkName . capitalize . ccStr
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
