module Distribution.Dev.Ghci
   ( actions )
where

import Control.Applicative ( (<$>), (<|>) )
import Data.List ( stripPrefix )
import Distribution.Simple.Program ( emptyProgramConfiguration
                                   , runProgram
                                   , requireProgram
                                   , ghcProgram
                                   )
import System.Console.GetOpt       ( OptDescr(..), ArgDescr(..) )

import Distribution.Dev.Command   ( CommandActions(..), CommandResult(..) )
import Distribution.Dev.Flags     ( Config, getVerbosity )
import Distribution.Dev.BuildOpts ( getBuildArgs )

actions :: CommandActions
actions = CommandActions
              { cmdDesc = "Run ghci configured as per the specified cabal file."
              , cmdRun = \cfg opts args -> invokeGhci cfg opts args
              , cmdOpts = [ Option "t" ["target"] (ReqArg id "TARGET") $
                            "Use TARGET executable or test suite's context " ++
                            "instead of the package."
                          ]
              , cmdPassFlags = True
              }

invokeGhci :: Config -> [String] -> [String] -> IO CommandResult
invokeGhci cfg opts args = do
  let v = getVerbosity cfg
  let target = foldl (const Just) Nothing opts
  res <- getBuildArgs cfg args

  case res >>= selectArgs target of
    Left err -> return $ CommandError err
    Right buildArgs -> do
      -- Use the arguments that cabal-install passed to GHC to
      -- invoke ghci instead
      let ghciArgs = "--interactive" : filter (/= "--make") buildArgs
      (ghc, _) <- requireProgram v ghcProgram emptyProgramConfiguration
      runProgram v ghc ghciArgs
      return CommandOk

-- |Select the GHC arguments for a given target if specified, the
-- package arguments if the package target is configured, or the first
-- set otherwise.
selectArgs :: Maybe String -> [[String]] -> Either String [String]
selectArgs targetName argsList = do
  byTarget <- argsByTarget argsList
  case targetName of
    Just t -> case lookup (Executable t) byTarget of
      Just as -> Right as
      Nothing -> Left ("No target " ++ t ++ " defined")
    Nothing -> case lookup Package byTarget of
      Just as -> Right as
      Nothing -> case byTarget of
        (x:_) -> Right $ snd x
        []    -> Left "Failed to extract GHC build arguments"

data Target = Package | Executable String
            deriving Eq

argsByTarget :: [[String]] -> Either String [(Target, [String])]
argsByTarget = mapM (\a -> fmap (flip (,) a) $ inferTarget a)
  where inferTarget args = case exec args <|> package args of
          Just t  -> Right t
          Nothing -> Left "Failed to infer target for GHC build arguments"

        exec args =
          case break (== "-o") args of
            (_, _:path:_) -> Executable . takeWhile (/= '/') <$>
                             stripPrefix "dist/build/" path
            (_, _)        -> Nothing
        package args =
          case break (== "-package-name") args of
            (_, _:_pkg:_) -> Just Package
            (_, _)        -> Nothing
