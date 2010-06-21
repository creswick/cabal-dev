{- Copyright (c) 2010 Galois, Inc -}
{-# LANGUAGE ExistentialQuantification #-}
module Main
    ( main )
where

import Control.Applicative ( (<$>) )
import Data.Maybe ( listToMaybe )
import Control.Monad ( liftM, ap, guard, forM_ )
import Control.Monad ( when, unless )
import System.Exit ( exitFailure, exitSuccess )
import System.FilePath ( splitExtension, splitDirectories, (<.>), (</>) )
import System.Environment ( getArgs, getProgName )
import System.Console.GetOpt ( OptDescr(..), ArgOrder(..), ArgDescr(..), getOpt', usageInfo, getOpt )
import System.Directory ( canonicalizePath, createDirectoryIfMissing )
import System.IO ( withFile, IOMode(..) )
import Distribution.Package ( PackageName(..), PackageIdentifier(..) )
import Distribution.Text ( simpleParse, display )
import qualified Data.ByteString.Lazy as L
import qualified Codec.Compression.GZip as Z
import qualified Codec.Archive.Tar as T

import Debug.Trace

data GlobalFlag = Help
                | Verbose
                | LocalRepo FilePath
                  deriving (Eq, Show)

globalOpts :: [OptDescr GlobalFlag]
globalOpts = [ Option "h?" ["help"] (NoArg Help) "Show help text"
             , Option "r" ["local-repo"] (ReqArg LocalRepo "DIR") "The location of the development cabal sandbox (default: ./cabal-dev)"
             , Option "v" ["verbose"] (NoArg Verbose) "Show debugging output"
             ]

parseGlobalFlags :: [String] -> ([GlobalFlag], [String], [String])
parseGlobalFlags args =
    let (flgs, args', unknown, errs) = getOpt' Permute globalOpts args
    in (flgs, args' ++ unknown, errs)

helpRequested :: [GlobalFlag] -> Bool
helpRequested = (Help `elem`)

globalUsage :: IO String
globalUsage = do
  progName <- getProgName
  let preamble =
          unlines $
          [ ""
          , "Usage: " ++ progName ++ " <command>"
          , ""
          , "Where <command> is one of:"
          ] ++ map ("  " ++) allCommandNames ++
          [ ""
          , "Options:"
          ]
  return $ usageInfo preamble globalOpts

data Command = MkRepo deriving (Enum, Bounded)

cmdName :: String -> Maybe Command
cmdName s = case s of
             "mk-repo" -> Just MkRepo
             _ -> Nothing

nameCmd :: Command -> String
nameCmd MkRepo = "mk-repo"

allCommandNames :: [String]
allCommandNames = map nameCmd [minBound..maxBound]

main :: IO ()
main = do
  (globalFlags, args, errs) <- parseGlobalFlags `fmap` getArgs
  unless (null errs) $ do
         mapM_ putStrLn errs
         putStr =<< globalUsage
         exitFailure

  case args of
    (name:args') ->
        case cmdName name of
          Just cmd -> do
              runCmd cmd globalFlags args'
          Nothing -> do putStrLn $ "Unknown command: " ++ show name
                        putStr =<< globalUsage
                        exitFailure
    _ | helpRequested globalFlags -> do
              putStr =<< globalUsage
              exitSuccess
      | otherwise -> do
              putStrLn "Missing command name"
              putStr =<< globalUsage
              exitFailure

runCmd :: Command -> [GlobalFlag] -> [String] -> IO ()
runCmd cmd flgs args
    | helpRequested flgs = showHelp
    | otherwise = run
    where
      cmdAct = case cmd of
                 MkRepo -> mkRepoAct

      showHelp = do putStrLn $ cmdDesc cmdAct
                    putStr =<< globalUsage
                    exitSuccess

      run = case cmdAct of
              (CommandActions _ r o) ->
                  do let (cmdFlags, cmdArgs, cmdErrs) =
                             getOpt Permute o args
                     unless (null cmdErrs) $ do
                          putStrLn $ cmdDesc cmdAct
                          putStr $ unlines cmdErrs
                          putStr =<< globalUsage
                          exitFailure
                     r flgs cmdFlags cmdArgs


data CommandActions
    = forall a . CommandActions
      { cmdDesc :: String
      , cmdRun :: [GlobalFlag] -> [a] -> [String] -> IO ()
      , cmdOpts :: [OptDescr a]
      }

mkRepoOpts :: [OptDescr ()]
mkRepoOpts = []

mkRepoAct :: CommandActions
mkRepoAct = CommandActions
            { cmdDesc = "Create a new repository from cabal package sources"
            , cmdRun = \flgs _ -> mkRepo flgs (getLocalRepo flgs)
            , cmdOpts = mkRepoOpts
            }

getLocalRepo :: [GlobalFlag] -> Maybe FilePath
getLocalRepo flgs = listToMaybe [ fn | LocalRepo fn <- flgs ]

data Level = Normal | Debug deriving (Eq, Ord)

logMsg :: Level -> [GlobalFlag] -> String -> IO ()
logMsg lvl flgs | shouldLog = putStrLn
                | otherwise = const $ return ()
    where
      shouldLog =
          case sum [ 1 | Verbose <- flgs ] of
            0 -> lvl <= Normal
            _ -> True

debug :: [GlobalFlag] -> String -> IO ()
debug = logMsg Debug

defaultLocalRepo :: FilePath
defaultLocalRepo = "./cabal-dev/packages"

resolveLocalRepo :: [GlobalFlag] -> Maybe FilePath -> IO FilePath
resolveLocalRepo flgs localRepoSpec = do
  relLocalRepo <-
      case localRepoSpec of
        Nothing -> do
          debug flgs $ "No local repository specified. Using " ++ defaultLocalRepo
          return defaultLocalRepo
        Just s -> return s

  localRepo <- canonicalizePath relLocalRepo
  debug flgs $ "Using " ++ localRepo ++ " as the local repository path"
  createDirectoryIfMissing True localRepo
  return localRepo

-- |Extract a cabal file from a package tarball
extractCabalFile :: T.Entries -> Maybe (PackageIdentifier, L.ByteString)
extractCabalFile = T.foldEntries step Nothing (const Nothing)
    where
      step ent Nothing = (,) `liftM` entPackageId ent `ap` entBytes ent
      step _   ans     = ans

      entPackageId ent =
          case splitDirectories $ T.entryPath ent of
            [d, f] ->
                do i <- simpleParse d
                   let (p, e) = splitExtension f
                   guard $ e == ".cabal" && (PackageName p) == pkgName i
                   return i
            _      -> Nothing

      entBytes ent = case T.entryContent ent of
                       T.NormalFile x _ -> return x
                       _ -> trace "3" Nothing

-- |The path to the .cabal file in the 00-index.tar file
indexName :: PackageIdentifier -> FilePath
indexName pkgId = display (pkgName pkgId) </>
                  display (pkgVersion pkgId) </>
                  (display (pkgName pkgId) <.> "cabal")

mkRepo :: [GlobalFlag] -> Maybe FilePath -> [String] -> IO ()
mkRepo flgs localRepoSpec fns = do
  localRepo <- resolveLocalRepo flgs localRepoSpec
  putStrLn $ "Making a cabal repo in " ++ localRepo ++ " out of " ++ show fns
  forM_ fns $ \fn ->
      let (fn1, ext1) = splitExtension fn
          (_, ext2) = splitExtension fn1
      in case (ext2, ext1) of
           (".tar", ".gz") ->
               withFile fn ReadMode $ \h ->
                   do ents <- T.read . Z.decompress <$> L.hGetContents h
                      case extractCabalFile ents of
                        Nothing -> error "No cabal file found"
                        Just (pkgId, cf) -> do
                            putStrLn $ indexName pkgId
                            error "got it!"
           _ -> error "Treat as a directory"
