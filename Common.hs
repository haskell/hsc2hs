module Common where

import Control.Exception        ( bracket_ )
import qualified Control.Exception as Exception
import Control.Monad            ( when )
import Data.Char                ( isSpace )
import Data.List                ( foldl' )
import System.IO

import System.Process           ( createProcess, proc, runProcess, waitForProcess )

import System.Exit              ( ExitCode(..), exitWith )
import System.Directory         ( removeFile )

die :: String -> IO a
die s = hPutStr stderr s >> exitWith (ExitFailure 1)

default_compiler :: String
default_compiler = "gcc"

------------------------------------------------------------------------
-- Write the output files.

writeBinaryFile :: FilePath -> String -> IO ()
writeBinaryFile fp str = withBinaryFile fp WriteMode $ \h -> hPutStr h str

rawSystemL :: FilePath -> String -> Bool -> FilePath -> [String] -> IO ()
rawSystemL outDir action flg prog args = withResponseFile outDir "c2hscall.rsp" args $ \rspFile -> do
  let cmdLine = prog++" "++unwords args
  when flg $ hPutStrLn stderr ("Executing: " ++ cmdLine)
  (_,_,_,ph) <- createProcess (proc prog ['@':rspFile])
  exitStatus <- waitForProcess ph
  case exitStatus of
    ExitFailure exitCode -> die $ action ++ " failed "
                               ++ "(exit code " ++ show exitCode ++ ")\n"
                               ++ "command was: " ++ cmdLine ++ "\n"
    _                    -> return ()


rawSystemWithStdOutL :: FilePath -> String -> Bool -> FilePath -> [String] -> FilePath -> IO ()
rawSystemWithStdOutL outDir action flg prog args outFile = withResponseFile outDir "c2hscall.rsp" args $ \rspFile -> do
  let cmdLine = prog++" "++unwords args++" >"++outFile
  when flg (hPutStrLn stderr ("Executing: " ++ cmdLine))
  hOut <- openFile outFile WriteMode
  process <- runProcess prog ['@':rspFile] Nothing Nothing Nothing (Just hOut) Nothing
  exitStatus <- waitForProcess process
  hClose hOut
  case exitStatus of
    ExitFailure exitCode -> die $ action ++ " failed "
                               ++ "(exit code " ++ show exitCode ++ ")\n"
                               ++ "command was: " ++ cmdLine ++ "\n"
    _                    -> return ()

-- delay the cleanup of generated files until the end; attempts to
-- get around intermittent failure to delete files which has
-- just been exec'ed by a sub-process (Win32 only.)
finallyRemove :: FilePath -> IO a -> IO a
finallyRemove fp act =
  bracket_ (return fp)
           (noisyRemove fp)
           act
 where
  noisyRemove fpath =
    catchIO (removeFile fpath)
            (\ e -> hPutStrLn stderr ("Failed to remove file " ++ fpath ++ "; error= " ++ show e))

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

onlyOne :: String -> IO a
onlyOne what = die ("Only one "++what++" may be specified\n")

-- response file handling borrowed from cabal's at Distribution.Simple.Program.ResponseFile

withTempFile :: FilePath    -- ^ Temp dir to create the file in
             -> String   -- ^ File name template. See 'openTempFile'.
             -> (FilePath -> Handle -> IO a) -> IO a
withTempFile tmpDir template action =
  Exception.bracket
    (openTempFile tmpDir template)
    (\(name, handle) -> do hClose handle
                           removeFile $ name)
    (uncurry action)

withResponseFile ::
     FilePath           -- ^ Working directory to create response file in.
  -> FilePath           -- ^ Template for response file name.
  -> [String]           -- ^ Arguments to put into response file.
  -> (FilePath -> IO a)
  -> IO a
withResponseFile workDir fileNameTemplate arguments f =
  withTempFile workDir fileNameTemplate $ \responseFileName hf -> do
    let responseContents = unlines $ map escapeResponseFileArg arguments
    hPutStr hf responseContents
    hClose hf
    f responseFileName

-- Support a gcc-like response file syntax.  Each separate
-- argument and its possible parameter(s), will be separated in the
-- response file by an actual newline; all other whitespace,
-- single quotes, double quotes, and the character used for escaping
-- (backslash) are escaped.  The called program will need to do a similar
-- inverse operation to de-escape and re-constitute the argument list.
escapeResponseFileArg :: String -> String
escapeResponseFileArg = reverse . foldl' escape []
  where
    escape :: String -> Char -> String
    escape cs c =
      case c of
        '\\'          -> c:'\\':cs
        '\''          -> c:'\\':cs
        '"'           -> c:'\\':cs
        _ | isSpace c -> c:'\\':cs
          | otherwise -> c:cs
