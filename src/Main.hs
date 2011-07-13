{- Copyright (c) 2010 Galois, Inc -}
module Main
    ( main )
where

import Data.Char ( isSpace, isLetter )
import Data.List ( intercalate, transpose, sort )
import Data.Maybe ( listToMaybe )
import Data.Version ( showVersion )
import Control.Monad ( unless )
import System.Exit ( exitWith, ExitCode(..) )
import System.Environment ( getArgs, getProgName )
import System.Console.GetOpt ( usageInfo, getOpt, ArgOrder(Permute) )
import Distribution.Simple.Utils ( cabalVersion, debug )
import Distribution.Text ( display )

import Distribution.Dev.Command ( CommandActions(..), CommandResult(..) )
import Distribution.Dev.Flags ( parseGlobalFlags, helpRequested, globalOpts
                              , GlobalFlag(Version), getOpt'', fromFlags
                              , getVerbosity, Config
                              )
import qualified Distribution.Dev.AddSource as AddSource
import qualified Distribution.Dev.Ghci as Ghci
import qualified Distribution.Dev.InvokeCabal as InvokeCabal
import qualified Distribution.Dev.InstallDependencies as InstallDeps
import qualified Distribution.Dev.CabalInstall as CI
import Paths_cabal_dev ( version )

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

  case [f|(Version f) <- globalFlags] of
    (True:_) -> printNumericVersion
    (False:_) -> printVersion
    [] -> return ()

  let cfg = fromFlags globalFlags
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
  let fmtCommands cmds = fmtTable "  " [ [[""], [n], wrap 60 d] | (n, _, d) <- cmds ]
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

-- XXX: this is very inefficient, to an extent that is noticable and
-- annoying sometimes. A directed search would be much better.
-- |Wrap a String of text to lines shorter than the specified number.
--
-- This function has heuristics for human-readability, such as
-- avoiding splitting in the middle of words when possible.
wrap :: Int -> String -> [String]
wrap w = snd . go 0
    where
      go n s
          | length s < w = (n, [s])
          | otherwise = minimum $
                        do -- consider the best 10 splits of a line
                          (sc, r, s') <- take 5 $ sort $ splits s
                          let (sc', t) = go (n + sc) s'
                          return (sc', r:t)

      splits s = map (\k -> score $ splitAt k s) [w - 1,w - 2..1]

      score ([], cs)      = (w * w, [], cs)
      score (r, [])       = (0, r, [])
      score (r, cs@(c:_)) = ( penalty (last r) c + w - length r
                            , r
                            , dropWhile isSpace cs
                            )
      penalty b c
          -- Don't penalize boundaries between space and non-space
          | not (isSpace b) && isSpace c  = 0
          | isSpace b && not (isSpace c)  = 2
          -- Penalize splitting a word heavily
          | isLetter b && isLetter c = w * 2
          -- Prefer splitting after a letter if it's not
          -- followed by a letter
          | isLetter c = 3
          -- Other kinds of splits are not as bad as splitting
          -- between words, but are still pretty harmful.
          | otherwise  = w `div` 2
