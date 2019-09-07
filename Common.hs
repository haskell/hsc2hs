{-# LANGUAGE CPP #-}
module Common where

import qualified Control.Exception as Exception
import Control.Monad            ( when )
import Data.Char                ( isSpace )
import Data.List                ( foldl' )
import System.IO
#if defined(mingw32_HOST_OS)
import Control.Concurrent       ( threadDelay )
import Data.Bits                ( xor )
import System.IO.Error          ( isPermissionError )
import System.CPUTime           ( getCPUTime )
import System.FilePath          ( (</>) )
#endif
import System.Process           ( createProcess, waitForProcess
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
             -> FilePath -- ^ Name of the hsc file being processed or template
             -> String   -- ^ Template for temp file
             -> Int      -- ^ Random seed for tmp name
             -> (FilePath -> Handle -> IO a) -> IO a
#if !defined(mingw32_HOST_OS)
withTempFile tmpDir _outBase template _seed action = do
  Exception.bracket
    (openTempFile tmpDir template)
    (\(name, handle) -> do hClose handle
                           removeFile $ name)
    (uncurry action)
#else
withTempFile tmpDir outBase template seed action = do
  -- openTempFile isn't atomic under Windows. This means it's unsuitable for
  -- use on Windows for creating random temp files.  Instead we'll try to create
  -- a reasonably random name based on the current outBase.  If the
  -- Sanity check to see that nothing invalidated this assumption is violated
  -- then we retry a few times otherwise an error is raised.
  rspFile <- findTmp 5
  Exception.bracket
    (openFile rspFile ReadWriteMode)
    (\handle -> finallyRemove rspFile $ hClose handle)
    (action rspFile)
    where findTmp :: Int -> IO FilePath
          findTmp 0 = die "Could not find unallocated temp file\n"
          findTmp n = do
            -- Generate a reasonable random number for token to prevent clashes if this
            -- function is used recursively.
            cpuTime <- getCPUTime
            let token = show $ (fromIntegral seed) `xor` cpuTime
                file = tmpDir </> (outBase ++ token ++ template)
            -- Because of the resolution of the CPU timers there exists a small
            -- possibility that multiple nested calls to withTempFile get the
            -- same "token".  To reduce the risk to almost zero we immediately
            -- create the file to reserve it.  If the file already exists we try
            -- again.
            res <- Exception.try $ openFile file ReadMode
            case (res :: Either Exception.SomeException Handle) of
              Left  _ -> return file
              Right h -> hClose h >> findTmp (n-1)
#endif

withResponseFile ::
     FilePath           -- ^ Working directory to create response file in.
  -> FilePath           -- ^ Template for response file name.
  -> [String]           -- ^ Arguments to put into response file.
  -> (FilePath -> IO a)
  -> IO a
withResponseFile workDir outBase arguments f =
  withTempFile workDir outBase "c2hscall.rsp" (length arguments) $ \responseFileName hf -> do
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
