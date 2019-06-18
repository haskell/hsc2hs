{-# LANGUAGE CPP #-}
module Common where

import Control.Concurrent       ( threadDelay )
import Control.Exception        ( bracket_, try, throw )
import qualified Control.Exception as Exception
import Control.Monad            ( when )
import System.IO
import System.IO.Error          ( isPermissionError )

import System.Process           ( rawSystem, createProcess_, waitForProcess
                                , proc, CreateProcess(..), StdStream(..) )

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

rawSystemL :: String -> Bool -> FilePath -> [String] -> IO ()
rawSystemL action flg prog args = do
  let cmdLine = prog++" "++unwords args
  when flg $ hPutStrLn stderr ("Executing: " ++ cmdLine)
  exitStatus <- rawSystem prog args
  case exitStatus of
    ExitFailure exitCode -> die $ action ++ " failed "
                               ++ "(exit code " ++ show exitCode ++ ")\n"
                               ++ "command was: " ++ cmdLine ++ "\n"
    _                    -> return ()

rawSystemWithStdOutL :: String -> Bool -> FilePath -> [String] -> FilePath -> IO ()
rawSystemWithStdOutL action flg prog args outFile = do
  let cmdLine = prog++" "++unwords args++" >"++outFile
  when flg (hPutStrLn stderr ("Executing: " ++ cmdLine))
  hOut <- openFile outFile WriteMode
  (_ ,_ ,_ , process) <-
    -- We use createProcess_ here instead of runProcess or createProcess since
    -- we need to specify a custom CreateProcess structure to turn on
    -- use_process_jobs when available and also because we don't want the
    -- handles closed automatically.  We close them manually after the process
    -- terminates.
    createProcess_ "rawSystemWithStdOutL"
#if MIN_VERSION_process (1,5,0)
      (proc prog args){ use_process_jobs = True, std_out = UseHandle  hOut }
#else
      (proc prog args){ std_out = UseHandle hOut }
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
  bracket_ (return fp)
           (noisyRemove fp)
           act
 where
  max_retries :: Int
  max_retries = 5

  noisyRemove :: FilePath -> IO ()
  noisyRemove fpath =
    catchIO (removeFileInternal max_retries fpath)
            (\ e -> hPutStrLn stderr ("Failed to remove file " ++ fpath ++ "; error= " ++ show e))
  removeFileInternal retries path = do
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
    res <- try $ removeFile path
    case res of
      Right a -> return a
      Left ex | isPermissionError ex && retries > 1 -> do
                  let retries' = retries - 1
                  threadDelay ((max_retries - retries') * 200)
                  removeFileInternal retries' path
              | otherwise -> throw ex
#else
    removeFile path
#endif

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

onlyOne :: String -> IO a
onlyOne what = die ("Only one "++what++" may be specified\n")
