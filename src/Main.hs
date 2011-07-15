{- Copyright (c) 2010 Galois, Inc -}
module Main
    ( main )
where

import Data.Char ( isSpace, isLetter )
import Data.List ( intercalate, transpose )
import Data.Maybe ( listToMaybe )
import Data.Version ( showVersion )
import Control.Monad ( unless, mplus )
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
import qualified Distribution.Dev.BuildOpts as BuildOpts
import qualified Distribution.Dev.CabalInstall as CI
import Paths_cabal_dev ( version )

import qualified Data.Map as Map
import Data.Map ( Map )

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

-- |Wrap a String of text to lines shorter than the specified number.
--
-- This function has heuristics for human-readability, such as
-- avoiding splitting in the middle of words when possible.
wrap :: Int -> String -> [String]
wrap w s = snd $ fst $ wrap' w Map.empty 0 s

-- |Ensure that the memo table has an entry for the specified location
-- in the string.
wrap' :: Int                     -- ^ Line length
      -> Map Int (Int, [String]) -- ^ Memo table
      -> Int                     -- ^ Position in String
      -> String                  -- ^ String remaining
      -> ((Int, [String]), Map Int (Int, [String])) -- ^ Updated memo table
wrap' w best i s =
    -- Check to see if this location is already in the memo table
    case Map.lookup i best of
      Nothing  -> go Nothing best $ splits w s
      Just ans -> (ans, best)

    where
      go Nothing _ []              = error "Wrapping failed"

      -- We have tried all of the options for splitting at this point.
      -- Now we have the best result, and we can and add it to the
      -- memo table.
      go (Just (sc, ans)) b []     = ((sc, ans), Map.insert i (sc, ans) b)

      -- Try a particular split
      go a b ((k, sc, l, rest):s') =
          let (b'', a') = case rest of
                            [] -> (b, Just (sc, [l]))
                            _  -> let i' = k + i
                                      ((sc', ans), b') = wrap' w b i' rest
                                  in (b', Just (sc + sc', l:ans))

              a'' = case (a, a') of
                      (Just (sc1, a1), Just (sc2, a2)) ->
                          Just $ if sc1 <= sc2 then (sc1, a1) else (sc2, a2)
                      _                                -> a `mplus` a'

          in go a'' b'' s'

-- Find all the locations that make sense to split the next line, and
-- score them.
splits :: Int -> String -> [(Int, Int, String, String)]
splits w s = map (\k -> score k $ splitAt k s) [w - 1,w - 2..1]
    where
      score k ([], cs)      = (k, w * w, [], cs)
      score k (r, [])       = (k, 0, r, [])
      score k (r, cs@(c:_)) = let (sps, cs') = countDropSpaces 0 cs
                              in ( k + sps
                                 , penalty (last r) c + w - length r
                                 , r
                                 , cs'
                                 )

      countDropSpaces i (c:cs) | isSpace c = countDropSpaces (i + 1) cs
      countDropSpaces i cs                 = (i, cs)

      -- Characters that want to stick to non-whitespace characters to
      -- their right
      rbind = (`elem` "\"'([<")

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
