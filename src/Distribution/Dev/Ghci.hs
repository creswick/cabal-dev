module Distribution.Dev.Ghci
   ( actions )
where

import Distribution.Simple.Program ( emptyProgramConfiguration
                                   , runProgram
                                   , requireProgram
                                   , ghcProgram
                                   )
import System.Console.GetOpt       ( OptDescr )

import Distribution.Dev.Command   ( CommandActions(..), CommandResult(..) )
import Distribution.Dev.Flags     ( Config, getVerbosity )
import Distribution.Dev.BuildOpts ( getBuildArgs )

actions :: CommandActions
actions = CommandActions
              { cmdDesc = "Run ghci configured as per the specified cabal file."
              , cmdRun = \cfg _ args -> invokeGhci cfg args
              , cmdOpts = [] :: [OptDescr ()]
              , cmdPassFlags = True
              }

invokeGhci :: Config -> [String] -> IO CommandResult
invokeGhci cfg args = do
  let v = getVerbosity cfg
  res <- getBuildArgs cfg args
  case res of
    Left err -> return $ CommandError err
    Right (buildArgs:_) ->
        do -- Use the arguments that cabal-install passed to GHC to
           -- invoke ghci instead
          let ghciArgs = "--interactive" : filter (/= "--make") buildArgs
          (ghc, _) <- requireProgram v ghcProgram emptyProgramConfiguration
          runProgram v ghc ghciArgs
          return CommandOk
    Right [] -> return $ CommandError "Failed to extract GHC build arguments"
