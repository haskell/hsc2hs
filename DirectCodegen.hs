{-# LANGUAGE CPP #-}
module DirectCodegen where

{-
The standard mode for hsc2hs: generates a C file which is
compiled and run; the output of that program is the .hs file.
-}

import Control.Exception        ( bracket_ )
import qualified Control.Exception as Exception
import Data.Char                ( isAlphaNum, isSpace, intToDigit,
                                  toUpper, ord )
import Data.List                ( intersperse )
import HSCParser                ( SourcePos(..), Token(..) )
import Control.Monad            ( when )
import System.IO

#if __GLASGOW_HASKELL__ >= 604
import System.Process           ( runProcess, waitForProcess )
#define HAVE_runProcess
#endif

import System.Cmd               ( rawSystem )
#ifndef HAVE_runProcess
import System.Cmd               ( system )
#endif

import System.Exit              ( ExitCode(..), exitWith )
import System.Directory         ( removeFile )

data Flag
    = Help
    | Version
    | Template  String
    | Compiler  String
    | Linker    String
    | CompFlag  String
    | LinkFlag  String
    | NoCompile
    | Include   String
    | Define    String (Maybe String)
    | Output    String
    | KeepFiles
    | Verbose

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

output :: [Flag] -> FilePath -> String -> [Token] -> IO ()
output flags compiler name toks = do

    (outName, outDir, outBase) <- case [f | Output f <- flags] of
        [] -> if not (null ext) && last ext == 'c'
                 then return (dir++base++init ext,  dir, base)
                 else
                    if ext == ".hs"
                       then return (dir++base++"_out.hs", dir, base)
                       else return (dir++base++".hs",     dir, base)
              where
               (dir,  file) = splitName name
               (base, ext)  = splitExt  file
        [f] -> let
            (dir,  file) = splitName f
            (base, _)    = splitExt file
            in return (f, dir, base)
        _ -> onlyOne "output file"

    let cProgName    = outDir++outBase++"_hsc_make.c"
        oProgName    = outDir++outBase++"_hsc_make.o"
        progName     = outDir++outBase++"_hsc_make"
#if defined(mingw32_HOST_OS) || defined(__CYGWIN32__)
-- This is a real hack, but the quoting mechanism used for calling the C preprocesseor
-- via GHC has changed a few times, so this seems to be the only way...  :-P * * *
                          ++ ".exe"
#endif
	outHFile     = outBase++"_hsc.h"
        outHName     = outDir++outHFile
        outCName     = outDir++outBase++"_hsc.c"

	beVerbose    = any (\ x -> case x of { Verbose -> True; _ -> False}) flags

    let execProgName
            | null outDir = dosifyPath ("./" ++ progName)
            | otherwise   = progName

    let specials = [(pos, key, arg) | Special pos key arg <- toks]

    let needsC = any (\(_, key, _) -> key == "def") specials
        needsH = needsC
        keepFiles = not $ null [() | KeepFiles <- flags]
        possiblyRemove = if keepFiles
                         then flip const
                         else finallyRemove

    let includeGuard = map fixChar outHName
            where
            fixChar c | isAlphaNum c = toUpper c
                      | otherwise    = '_'

    linker <- case [l | Linker l <- flags] of
        []  -> return compiler
        ls  -> return (last ls)

    writeBinaryFile cProgName $
        concatMap outFlagHeaderCProg flags++
        concatMap outHeaderCProg specials++
        "\nint main (int argc, char *argv [])\n{\n"++
        outHeaderHs flags (if needsH then Just outHName else Nothing) specials++
        outHsLine (SourcePos name 0)++
        concatMap outTokenHs toks++
        "    return 0;\n}\n"

    -- NOTE: hbc compiles "[() | NoCompile <- flags]" into wrong code,
    -- so we use something slightly more complicated.   :-P
    when (any (\x -> case x of NoCompile -> True; _ -> False) flags) $
       exitWith ExitSuccess

    rawSystemL ("compiling " ++ cProgName) beVerbose compiler
	(  ["-c"]
        ++ [cProgName]
        ++ ["-o", oProgName]
        ++ [f | CompFlag f <- flags]
	)
    possiblyRemove cProgName $ do

      rawSystemL ("linking " ++ oProgName) beVerbose linker
        (  [oProgName]
        ++ ["-o", progName]
        ++ [f | LinkFlag f <- flags]
	)
      possiblyRemove oProgName $ do

        rawSystemWithStdOutL ("running " ++ execProgName) beVerbose execProgName [] outName
        possiblyRemove progName $ do

          when needsH $ writeBinaryFile outHName $
            "#ifndef "++includeGuard++"\n" ++
            "#define "++includeGuard++"\n" ++
            "#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ < 409\n" ++
            "#include <Rts.h>\n" ++
            "#endif\n" ++
            "#include <HsFFI.h>\n" ++
            "#if __NHC__\n" ++
            "#undef HsChar\n" ++
            "#define HsChar int\n" ++
            "#endif\n" ++
            concatMap outFlagH flags++
            concatMap outTokenH specials++
            "#endif\n"

          when needsC $ writeBinaryFile outCName $
            "#include \""++outHFile++"\"\n"++
            concatMap outTokenC specials
            -- NB. outHFile not outHName; works better when processed
            -- by gcc or mkdependC.

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
#ifndef HAVE_runProcess
  exitStatus <- system cmdLine
#else
  hOut <- openFile outFile WriteMode
  process <- runProcess prog args Nothing Nothing Nothing (Just hOut) Nothing
  exitStatus <- waitForProcess process
  hClose hOut
#endif
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

outFlagHeaderCProg :: Flag -> String
outFlagHeaderCProg (Template t)          = "#include \""++t++"\"\n"
outFlagHeaderCProg (Include  f)          = "#include "++f++"\n"
outFlagHeaderCProg (Define   n Nothing)  = "#define "++n++" 1\n"
outFlagHeaderCProg (Define   n (Just v)) = "#define "++n++" "++v++"\n"
outFlagHeaderCProg _                     = ""

outHeaderCProg :: (SourcePos, String, String) -> String
outHeaderCProg (pos, key, arg) = case key of
    "include"           -> outCLine pos++"#include "++arg++"\n"
    "define"            -> outCLine pos++"#define "++arg++"\n"
    "undef"             -> outCLine pos++"#undef "++arg++"\n"
    "def"               -> case arg of
        's':'t':'r':'u':'c':'t':' ':_ -> outCLine pos++arg++"\n"
        't':'y':'p':'e':'d':'e':'f':' ':_ -> outCLine pos++arg++"\n"
        _ -> ""
    _ | conditional key -> outCLine pos++"#"++key++" "++arg++"\n"
    "let"               -> case break (== '=') arg of
        (_,      "")     -> ""
        (header, _:body) -> case break isSpace header of
            (name, args) ->
                outCLine pos++
                "#define hsc_"++name++"("++dropWhile isSpace args++") " ++
                "printf ("++joinLines body++");\n"
    _ -> ""
   where
    joinLines = concat . intersperse " \\\n" . lines

outHeaderHs :: [Flag] -> Maybe String -> [(SourcePos, String, String)] -> String
outHeaderHs flags inH toks =
    "#if " ++
    "__GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ < 409\n" ++
    "    printf (\"{-# OPTIONS -optc-D" ++
    "__GLASGOW_HASKELL__=%d #-}\\n\", " ++
    "__GLASGOW_HASKELL__);\n" ++
    "#endif\n"++
    case inH of
        Nothing -> concatMap outFlag flags++concatMap outSpecial toks
        Just f  -> outInclude ("\""++f++"\"")
    where
    outFlag (Include f)          = outInclude f
    outFlag (Define  n Nothing)  = outOption ("-optc-D"++n)
    outFlag (Define  n (Just v)) = outOption ("-optc-D"++n++"="++v)
    outFlag _                    = ""
    outSpecial (pos, key, arg) = case key of
        "include"                  -> outInclude arg
        "define" | goodForOptD arg -> outOption ("-optc-D"++toOptD arg)
                 | otherwise       -> ""
        _ | conditional key        -> outCLine pos++"#"++key++" "++arg++"\n"
        _                          -> ""
    goodForOptD arg = case arg of
        ""              -> True
        c:_ | isSpace c -> True
        '(':_           -> False
        _:s             -> goodForOptD s
    toOptD arg = case break isSpace arg of
        (name, "")      -> name
        (name, _:value) -> name++'=':dropWhile isSpace value
    outOption s =
	"#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ < 603\n" ++
	"    printf (\"{-# OPTIONS %s #-}\\n\", \""++
                  showCString s++"\");\n"++
	"#else\n"++
	"    printf (\"{-# OPTIONS_GHC %s #-}\\n\", \""++
                  showCString s++"\");\n"++
	"#endif\n"
    outInclude s =
	"#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ < 603\n" ++
	"    printf (\"{-# OPTIONS -#include %s #-}\\n\", \""++
                  showCString s++"\");\n"++
	"#elif __GLASGOW_HASKELL__ < 610\n"++
	"    printf (\"{-# INCLUDE %s #-}\\n\", \""++
                  showCString s++"\");\n"++
	"#endif\n"

outTokenHs :: Token -> String
outTokenHs (Text pos txt) =
    case break (== '\n') txt of
        (allTxt, [])       -> outText allTxt
        (first, _:rest) ->
            outText (first++"\n")++
            outHsLine pos++
            outText rest
    where
    outText s = "    fputs (\""++showCString s++"\", stdout);\n"
outTokenHs (Special pos key arg) =
    case key of
        "include"           -> ""
        "define"            -> ""
        "undef"             -> ""
        "def"               -> ""
        _ | conditional key -> outCLine pos++"#"++key++" "++arg++"\n"
        "let"               -> ""
        "enum"              -> outCLine pos++outEnum arg
        _                   -> outCLine pos++"    hsc_"++key++" ("++arg++");\n"

parseEnum :: String -> Maybe (String,String,[(Maybe String,String)])
parseEnum arg =
    case break (== ',') arg of
        (_, [])        -> Nothing
        (t, _:afterT) -> case break (== ',') afterT of
            (f, afterF) -> let
                enums []    = []
                enums (_:s) = case break (== ',') s of
                    (enum, rest) -> let
                        this = case break (== '=') $ dropWhile isSpace enum of
                            (name, []) -> (Nothing, name)
                            (hsName, _:cName) -> (Just hsName, cName)
                        in this:enums rest
                in Just (t, f, enums afterF)

outEnum :: String -> String
outEnum arg = case parseEnum arg of
    Nothing -> ""
    Just (t,f,enums) ->
        flip concatMap enums $ \(maybeHsName, cName) ->
            case maybeHsName of
               Nothing ->
                    "    hsc_enum ("++t++", "++f++", " ++
                    "hsc_haskellize (\""++cName++"\"), "++
                    cName++");\n"
               Just hsName ->
                    "    hsc_enum ("++t++", "++f++", " ++
                    "printf (\"%s\", \""++hsName++"\"), "++
                    cName++");\n"

outFlagH :: Flag -> String
outFlagH (Include  f)          = "#include "++f++"\n"
outFlagH (Define   n Nothing)  = "#define "++n++" 1\n"
outFlagH (Define   n (Just v)) = "#define "++n++" "++v++"\n"
outFlagH _                     = ""

outTokenH :: (SourcePos, String, String) -> String
outTokenH (pos, key, arg) =
    case key of
        "include" -> outCLine pos++"#include "++arg++"\n"
        "define"  -> outCLine pos++"#define " ++arg++"\n"
        "undef"   -> outCLine pos++"#undef "  ++arg++"\n"
        "def"     -> outCLine pos++case arg of
            's':'t':'r':'u':'c':'t':' ':_ -> arg++"\n"
            't':'y':'p':'e':'d':'e':'f':' ':_ -> arg++"\n"
            'i':'n':'l':'i':'n':'e':' ':_ ->
                "#ifdef __GNUC__\n" ++
                "extern\n" ++
                "#endif\n"++
                arg++"\n"
            _ -> "extern "++header++";\n"
          where header = takeWhile (\c -> c /= '{' && c /= '=') arg
        _ | conditional key -> outCLine pos++"#"++key++" "++arg++"\n"
        _ -> ""

outTokenC :: (SourcePos, String, String) -> String
outTokenC (pos, key, arg) =
    case key of
        "def" -> case arg of
            's':'t':'r':'u':'c':'t':' ':_ -> ""
            't':'y':'p':'e':'d':'e':'f':' ':_ -> ""
            'i':'n':'l':'i':'n':'e':' ':arg' ->
		case span (\c -> c /= '{' && c /= '=') arg' of
		(header, body) ->
		    outCLine pos++
		    "#ifndef __GNUC__\n" ++
		    "extern inline\n" ++
		    "#endif\n"++
		    header++
		    "\n#ifndef __GNUC__\n" ++
		    ";\n" ++
		    "#else\n"++
		    body++
		    "\n#endif\n"
            _ -> outCLine pos++arg++"\n"
        _ | conditional key -> outCLine pos++"#"++key++" "++arg++"\n"
        _ -> ""

conditional :: String -> Bool
conditional "if"      = True
conditional "ifdef"   = True
conditional "ifndef"  = True
conditional "elif"    = True
conditional "else"    = True
conditional "endif"   = True
conditional "error"   = True
conditional "warning" = True
conditional _         = False

outCLine :: SourcePos -> String
outCLine (SourcePos name line) =
    "#line "++show line++" \""++showCString (snd (splitName name))++"\"\n"

outHsLine :: SourcePos -> String
outHsLine (SourcePos name line) =
    "    hsc_line ("++show (line + 1)++", \""++
    showCString name++"\");\n"

showCString :: String -> String
showCString = concatMap showCChar
    where
    showCChar '\"' = "\\\""
    showCChar '\'' = "\\\'"
    showCChar '?'  = "\\?"
    showCChar '\\' = "\\\\"
    showCChar c | c >= ' ' && c <= '~' = [c]
    showCChar '\a' = "\\a"
    showCChar '\b' = "\\b"
    showCChar '\f' = "\\f"
    showCChar '\n' = "\\n\"\n           \""
    showCChar '\r' = "\\r"
    showCChar '\t' = "\\t"
    showCChar '\v' = "\\v"
    showCChar c    = ['\\',
                      intToDigit (ord c `quot` 64),
                      intToDigit (ord c `quot` 8 `mod` 8),
                      intToDigit (ord c          `mod` 8)]

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

