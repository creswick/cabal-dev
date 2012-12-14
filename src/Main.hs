{- Copyright (c) 2011 Galois, Inc -}
module Main
    ( main )
where

import Data.Char ( isSpace, isLetter )
import Data.List ( intercalate, transpose )
import Data.Maybe ( listToMaybe )
import Data.Version ( showVersion )
import Control.Monad ( unless )
import System.Exit ( exitWith, ExitCode(..) )
import System.Environment ( getArgs, getProgName, getEnv )
import System.IO.Error ( catchIOError )
import System.SetEnv (setEnv)
import System.FilePath ((</>), searchPathSeparator)
import System.Console.GetOpt ( usageInfo, getOpt, ArgOrder(Permute) )
import Distribution.Simple.Utils ( cabalVersion, debug )
import Distribution.Text ( display )
import Control.Monad.Trans.State ( evalState, gets, modify )

import Distribution.Dev.Command ( CommandActions(..), CommandResult(..) )
import Distribution.Dev.Flags ( parseGlobalFlags, helpRequested, globalOpts
                              , GlobalFlag(Version), getOpt'', fromFlags
                              , getVerbosity, Config, getSandbox
                              )
import qualified Distribution.Dev.AddSource as AddSource
import qualified Distribution.Dev.Ghci as Ghci
import qualified Distribution.Dev.InvokeCabal as InvokeCabal
import qualified Distribution.Dev.InstallDependencies as InstallDeps
import qualified Distribution.Dev.BuildOpts as BuildOpts
import qualified Distribution.Dev.GhcPkg as GhcPkg
import qualified Distribution.Dev.CabalInstall as CI
import Paths_cabal_dev ( version )

import qualified Data.Map as Map

cabalDevCommands :: [(String, CommandActions, String)]
cabalDevCommands = [ ( "add-source"
                     , AddSource.actions
                     , "Make a package available for cabal-install to " ++
                       "install (in a sandbox-local Hackage repository). " ++
                       "Note that this command does NOT install the " ++
                       "package in the sandbox, and does require the " ++
                       "package to have a working sdist."
                     )
                   , ( "install-deps"
                     , InstallDeps.actions
                     , "Install the packages that depend on the specified " ++
                       "packages, but not the packages themselves. " ++
                       "(Equivalent to install --only-dependencies for " ++
                       "cabal-install > 0.10)"
                     )
                   , ( "ghci"
                     , Ghci.actions
                     , "Start ghci with the sandbox's GHC package " ++
                       "repository available. This command does not " ++
                       "yet take into account the contents of the .cabal " ++
                       "file (e.g. source directories, available packages " ++
                       "LANGUAGE pragmas)."
                     )
                   , ( "buildopts"
                     , BuildOpts.actions
                     , "Extract the options that would be passed to the " ++
                       "compiler when building"
                     )
                   , ( "ghc-pkg"
                     , GhcPkg.actions
                     , "Invoke ghc-pkg including the appropriate " ++
                       "--package-conf argument to run on the sandbox's " ++
                       "package database."
                     )
                   ]

cabalInstallCommands :: [(String, CommandActions)]
cabalInstallCommands = map cabal CI.allCommands
    where
      cabal s = (CI.commandToString s, InvokeCabal.actions s)

allCommands :: [(String, CommandActions)]
allCommands = [(s, a) | (s, a, _) <- cabalDevCommands] ++ cabalInstallCommands

printVersion :: IO ()
printVersion = do
  putStr versionString
  exitWith ExitSuccess

versionString :: String
versionString = unlines $
                [ "cabal-dev " ++ showVersion version
                , "built with Cabal " ++ display cabalVersion
                ]

printNumericVersion :: IO ()
printNumericVersion = do
  putStrLn $ showVersion version
  exitWith ExitSuccess

main :: IO ()
main = do
  (globalFlags, args, errs) <- parseGlobalFlags `fmap` getArgs
  unless (null errs) $ do
         mapM_ putStrLn errs
         putStr =<< globalUsage
         exitWith (ExitFailure 1)

  let cfg = fromFlags globalFlags

  -- add sandbox bin dir to PATH, so that custom preprocessors that are
  -- installed into the sandbox are found
  let binDir = getSandbox cfg </> "bin"
  mPath <- maybeGetEnv "PATH"
  let path = maybe binDir ((binDir ++ [searchPathSeparator]) ++) mPath
  setEnv "PATH" path

  case [f|(Version f) <- globalFlags] of
    (True:_) -> printNumericVersion
    (False:_) -> printVersion
    [] -> return ()

  debug (getVerbosity cfg) versionString

  case args of
    (name:args') ->
        case nameCmd name of
          Just cmdAct | helpRequested globalFlags ->
                          do putStrLn $ cmdDesc cmdAct
                             putStr =<< globalUsage
                             exitWith ExitSuccess
                      | otherwise -> runCmd cmdAct cfg args'

          Nothing -> do putStrLn $ "Unknown command: " ++ show name
                        putStr =<< globalUsage
                        exitWith (ExitFailure 1)
    _ | helpRequested globalFlags -> do
              putStr =<< globalUsage
              exitWith ExitSuccess
      | otherwise -> do
              putStrLn "Missing command name"
              putStr =<< globalUsage
              exitWith (ExitFailure 1)

globalUsage :: IO String
globalUsage = do
  progName <- getProgName
  let fmtCommands cmds =
          fmtTable "  " [ [[""], [n], wrap 60 d] | (n, _, d) <- cmds ]
  let preamble =
          unlines $
          [ ""
          , "Usage: " ++ progName ++ " <command>"
          , ""
          , "Where <command> is one of:"
          ] ++ fmtCommands cabalDevCommands ++
          [ ""
          , "or any cabal-install command (see cabal --help for documentation)."
          , ""
          , "Options to cabal-dev:"
          ]
  return $ usageInfo preamble globalOpts

nameCmd :: String -> Maybe CommandActions
nameCmd s = listToMaybe [a | (n, a) <- allCommands, n == s]

runCmd :: CommandActions -> Config -> [String] -> IO ()
runCmd cmdAct cfg args =
    do res <- run
       case res of
         CommandOk        -> exitWith ExitSuccess
         CommandError msg -> showError [msg]
    where
      showError msgs = do
        putStr $ unlines $ "FAILED:":msgs ++ [replicate 50 '-', cmdDesc cmdAct]
        putStr =<< globalUsage
        exitWith (ExitFailure 1)

      run = case cmdAct of
              (CommandActions _ r o passFlags) ->
                  let (cmdFlags, cmdArgs, cmdErrs) =
                          if passFlags
                          then getOpt'' o args
                          else getOpt Permute o args
                  in if null cmdErrs
                     then r cfg cmdFlags cmdArgs
                     else showError cmdErrs

-- |Format a table
fmtTable :: String       -- ^Column separator
         -> [[[String]]] -- ^Table rows (each cell may have more than
                         -- one line)
         -> [String]     -- ^Lines of output
fmtTable colSep rows = map fmtLine $ fmtRow =<< rows
    where
      fmtRow cs = transpose $ map (pad [] (maximum $ map length cs)) cs
      fmtLine l = intercalate colSep $ zipWith (pad ' ') widths l
      widths = map (maximum . map length . concat) $ transpose rows
      pad c w s = take w $ s ++ repeat c

-- |Wrap a String of text to lines shorter than the specified number.
--
-- This function has heuristics for human-readability, such as
-- avoiding splitting in the middle of words when possible.
wrap :: Int      -- ^Maximum line length
     -> String   -- ^Text to wrap
     -> [String] -- ^Wrapped lines
wrap _ ""   = []
wrap w orig = snd $ evalState (go 0 orig) Map.empty
    where
      go loc s = do
        precomputed <- gets $ Map.lookup loc
        case precomputed of
          Nothing     -> bestAnswer loc =<< mapM (scoreSplit loc) (splits w s)
          Just answer -> return answer

      scoreSplit loc (offset, lineScore, line, s') =
          case s' of
            "" -> return (lineScore, [line])
            _  -> do
              (restScore, lines_) <- go (loc + offset) s'
              return (lineScore + restScore, line:lines_)

      bestAnswer _ [] = error "No splits found!"
      bestAnswer loc answers = do
        let answer = minimum answers
        modify $ Map.insert loc answer
        return answer

-- Find all the locations that make sense to split the next line, and
-- score them.
splits :: Int -> String -> [(Int, Int, String, String)]
splits w s = map (\k -> score k $ splitAt k s) [w - 1,w - 2..1]
    where
      score k ([], cs)      = (k, w * w, [], cs)
      score k (r, [])       = (k, 0, r, [])
      score k (r, cs@(c:_)) = let (sps, cs') = countDropSpaces 0 cs
                                  spaceLeft = w - length r
                              in ( -- How much of the string was consumed?
                                   k + sps

                                 -- How much does it cost to split here?
                                 , penalty (last r) c + spaceLeft * spaceLeft

                                 -- The text of the line that results
                                 -- from this split
                                 , r

                                 -- The text that has not yet been split
                                 , cs'
                                 )

      countDropSpaces i (c:cs) | isSpace c = countDropSpaces (i + 1) cs
      countDropSpaces i cs                 = (i, cs)

      -- Characters that want to stick to non-whitespace characters to
      -- their right
      rbind = (`elem` "\"'([<")

      -- How much should it cost to make a split between these
      -- characters?
      penalty b c
          -- Don't penalize boundaries between space and non-space
          | not (isSpace b) && isSpace c  = 0
          | isSpace b && not (isSpace c)  = 2
          | rbind b && not (isSpace c) = w `div` 2
          -- Penalize splitting a word heavily
          | isLetter b && isLetter c = w * 2
          -- Prefer splitting after a letter if it's not
          -- followed by a letter
          | isLetter c = 3
          -- Other kinds of splits are not as bad as splitting
          -- between words, but are still pretty harmful.
          | otherwise  = w

-- Return the value of the environment variable with the given name
-- or Nothing if the variable is not defined.
-- Probably should be replaced with System.Environment.lookupEnv
-- when the base library is upgraded >= 4.6
maybeGetEnv :: String -> IO (Maybe String)
maybeGetEnv name = (Just `fmap` getEnv name) `catchIOError` const (return Nothing)
