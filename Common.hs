{-# LANGUAGE CPP #-}
module Common where

import qualified Control.Exception as Exception
import Control.Monad            ( when )
import Data.Char                ( isSpace )
import Data.List                ( foldl' )
import System.IO
#if defined(mingw32_HOST_OS)
import Control.Concurrent       ( threadDelay )
import System.IO.Error          ( isPermissionError )
#endif
import System.Process           ( createProcess, waitForProcess
                                , proc, CreateProcess(..), StdStream(..) )
import System.Exit              ( ExitCode(..), exitWith )
import System.Directory         ( removeFile )
import System.FilePath          ( (</>) )

die :: String -> IO a
die s = hPutStr stderr s >> exitWith (ExitFailure 1)

default_compiler :: String
default_compiler = "gcc"

------------------------------------------------------------------------
-- Write the output files.

writeBinaryFile :: FilePath -> String -> IO ()
writeBinaryFile fp str = withBinaryFile fp WriteMode $ \h -> hPutStr h str

rawSystemL :: FilePath -> FilePath -> String -> Bool -> FilePath -> [String] -> IO ()
rawSystemL outDir outBase action flg prog args = withResponseFile outDir outBase args $ \rspFile -> do
  let cmdLine = prog++" "++unwords args
  when flg $ hPutStrLn stderr ("Executing: " ++ cmdLine)
  (_,_,_,ph) <- createProcess (proc prog ['@':rspFile])
  -- Because of the response files being written and removed after the process
  -- terminates we now need to use process jobs here to correctly wait for all
  -- child processes to terminate.  Not doing so would causes a race condition
  -- between the last child dieing and not holding a lock on the response file
  -- and the response file getting deleted.
#if MIN_VERSION_process (1,5,0)
    { use_process_jobs = True }
#endif
  exitStatus <- waitForProcess ph
  case exitStatus of
    ExitFailure exitCode -> die $ action ++ " failed "
                               ++ "(exit code " ++ show exitCode ++ ")\n"
                               ++ "command was: " ++ cmdLine ++ "\n"
    _                    -> return ()


rawSystemWithStdOutL :: FilePath -> FilePath -> String -> Bool -> FilePath -> [String] -> FilePath -> IO ()
rawSystemWithStdOutL outDir outBase action flg prog args outFile = withResponseFile outDir outBase args $ \rspFile -> do
  let cmdLine = prog++" "++unwords args++" >"++outFile
  when flg (hPutStrLn stderr ("Executing: " ++ cmdLine))
  hOut <- openFile outFile WriteMode
  (_ ,_ ,_ , process) <-
    -- We use createProcess here instead of runProcess since we need to specify
    -- a custom CreateProcess structure to turn on use_process_jobs when
    -- available.
    createProcess
#if MIN_VERSION_process (1,5,0)
      (proc prog ['@':rspFile]){ use_process_jobs = True, std_out = UseHandle  hOut }
#else
      (proc prog ['@':rspFile]){ std_out = UseHandle hOut }
#endif
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
  Exception.bracket_ (return fp)
           (noisyRemove fp)
           act
 where
  max_retries :: Int
  max_retries = 5

  noisyRemove :: FilePath -> IO ()
  noisyRemove fpath =
    catchIO (removeFileInternal max_retries fpath)
            (\ e -> hPutStrLn stderr ("Failed to remove file " ++ fpath ++ "; error= " ++ show e))
  removeFileInternal _retries path = do
#if defined(mingw32_HOST_OS)
  -- On Windows we have to retry the delete a couple of times.
  -- The reason for this is that a FileDelete command just marks a
  -- file for deletion. The file is really only removed when the last
  -- handle to the file is closed. Unfortunately there are a lot of
  -- system services that can have a file temporarily opened using a shared
  -- read-only lock, such as the built in AV and search indexer.
  --
  -- We can't really guarantee that these are all off, so what we can do is
  -- whenever after an rm the file still exists to try again and wait a bit.
    res <- Exception.try $ removeFile path
    case res of
      Right a -> return a
      Left ex | isPermissionError ex && _retries > 1 -> do
                  let retries' = _retries - 1
                  threadDelay ((max_retries - retries') * 200)
                  removeFileInternal retries' path
              | otherwise -> Exception.throw ex
#else
    removeFile path
#endif

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

onlyOne :: String -> IO a
onlyOne what = die ("Only one "++what++" may be specified\n")

-- response file handling borrowed from cabal's at Distribution.Simple.Program.ResponseFile

withTempFile :: FilePath -- ^ Temp dir to create the file in
             -> FilePath -- ^ Name of the hsc file being processed
             -> (FilePath -> Handle -> IO a) -> IO a
withTempFile tmpDir outBase action =
  -- openTempFile isn't atomic under Windows until GHC 8.10. This means it's
  -- unsuitable for use on Windows for creating random temp files.  For hsc2hs
  -- this doesn't matter much since hsc2hs is single threaded and always
  -- finishes one part of its compilation pipeline before moving on to the next.
  -- This means we can just use a deterministic file as a temp file.  This file
  -- will always be cleaned up before we move on to the next phase so we would
  -- never get a clash.  This follows the same pattern as in DirectCodegen.hs.
  Exception.bracket
    (openFile rspFile ReadWriteMode)
    (\handle -> finallyRemove rspFile $ hClose handle)
    (action rspFile)
    where rspFile = tmpDir </> (outBase ++"_hsc_make.rsp")

withResponseFile ::
     FilePath           -- ^ Working directory to create response file in.
  -> FilePath           -- ^ Template for response file name.
  -> [String]           -- ^ Arguments to put into response file.
  -> (FilePath -> IO a)
  -> IO a
withResponseFile workDir outBase arguments f =
  withTempFile workDir outBase $ \responseFileName hf -> do
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
