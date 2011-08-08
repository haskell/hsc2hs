{-# LANGUAGE CPP #-}
module Common where

import Control.Exception        ( bracket_ )
import qualified Control.Exception as Exception
import Control.Monad            ( when )
import System.IO

import System.Process           ( runProcess, waitForProcess )

import System.Cmd               ( rawSystem )

import System.Exit              ( ExitCode(..), exitWith )
import System.Directory         ( removeFile )

die :: String -> IO a
die s = hPutStr stderr s >> exitWith (ExitFailure 1)

default_compiler :: String
default_compiler = "gcc"

------------------------------------------------------------------------
-- Write the output files.

splitName :: String -> (String, String)
splitName name =
    case break (== '/') name of
        (file, [])       -> ([], file)
        (dir,  sep:rest) -> (dir++sep:restDir, restFile)
            where
            (restDir, restFile) = splitName rest

splitExt :: String -> (String, String)
splitExt name =
    case break (== '.') name of
        (base, [])         -> (base, [])
        (base, sepRest@(sep:rest))
            | null restExt -> (base,               sepRest)
            | otherwise    -> (base++sep:restBase, restExt)
            where
            (restBase, restExt) = splitExt rest

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
  process <- runProcess prog args Nothing Nothing Nothing (Just hOut) Nothing
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

-----------------------------------------
-- Modified version from ghc/compiler/SysTools
-- Convert paths foo/baz to foo\baz on Windows

subst :: Char -> Char -> String -> String
#if defined(mingw32_HOST_OS) || defined(__CYGWIN32__)
subst a b = map (\x -> if x == a then b else x)
#else
subst _ _ = id
#endif

dosifyPath :: String -> String
dosifyPath = subst '/' '\\'

unDosifyPath :: String -> String
unDosifyPath = subst '\\' '/'

