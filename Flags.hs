
module Flags where

import System.Console.GetOpt

data Mode
    = Help
    | Version
    | UseConfig Config

data Config = Config {
                  cTemplate :: Maybe FilePath,
                  cCompiler :: Maybe FilePath,
                  cLinker   :: Maybe FilePath,
                  cKeepFiles :: Bool,
                  cCrossCompile :: Bool,
                  cVerbose :: Bool,
                  cFlags :: [Flag]
              }

emptyMode :: Mode
emptyMode = UseConfig $ Config {
                            cTemplate = Nothing,
                            cCompiler = Nothing,
                            cLinker   = Nothing,
                            cKeepFiles = False,
                            cCrossCompile = False,
                            cVerbose = False,
                            cFlags = []
                        }

data Flag
    = CompFlag  String
    | LinkFlag  String
    | NoCompile
    | CrossSafe
    | Include   String
    | Define    String (Maybe String)
    | Output    String
    deriving Show

options :: [OptDescr (Mode -> Mode)]
options = [
    Option ['o'] ["output"]     (ReqArg (addFlag . Output)     "FILE")
        "name of main output file",
    Option ['t'] ["template"]   (ReqArg (withConfig . setTemplate)   "FILE")
        "template file",
    Option ['c'] ["cc"]         (ReqArg (withConfig . setCompiler)   "PROG")
        "C compiler to use",
    Option ['l'] ["ld"]         (ReqArg (withConfig . setLinker)     "PROG")
        "linker to use",
    Option ['C'] ["cflag"]      (ReqArg (addFlag . CompFlag)   "FLAG")
        "flag to pass to the C compiler",
    Option ['I'] []             (ReqArg (addFlag . CompFlag . ("-I"++)) "DIR")
        "passed to the C compiler",
    Option ['L'] ["lflag"]      (ReqArg (addFlag . LinkFlag)   "FLAG")
        "flag to pass to the linker",
    Option ['i'] ["include"]    (ReqArg (addFlag . include)    "FILE")
        "as if placed in the source",
    Option ['D'] ["define"]     (ReqArg (addFlag . define) "NAME[=VALUE]")
        "as if placed in the source",
    Option []    ["no-compile"] (NoArg  (addFlag NoCompile))
        "stop after writing *_hsc_make.c",
    Option ['x'] ["cross-compile"] (NoArg (withConfig $ setCrossCompile True))
        "activate cross-compilation mode",
    Option [] ["cross-safe"] (NoArg (addFlag CrossSafe))
        "restrict .hsc directives to those supported by --cross-compile",
    Option ['k'] ["keep-files"] (NoArg (withConfig $ setKeepFiles True))
        "do not remove temporary files",
    Option ['v'] ["verbose"]    (NoArg  (withConfig $ setVerbose True))
        "dump commands to stderr",
    Option ['?'] ["help"]       (NoArg  (setMode Help))
        "display this help and exit",
    Option ['V'] ["version"]    (NoArg  (setMode Version))
        "output version information and exit" ]

addFlag :: Flag -> Mode -> Mode
addFlag f (UseConfig c) = UseConfig $ c { cFlags = f : cFlags c }
addFlag _ mode = mode

setMode :: Mode -> Mode -> Mode
setMode Help           _    = Help
setMode _              Help = Help
setMode Version        _    = Version
setMode (UseConfig {}) _    = error "setMode: UseConfig: Can't happen"

withConfig :: (Config -> Config) -> Mode -> Mode
withConfig f (UseConfig c) = UseConfig $ f c
withConfig _ m = m

setTemplate :: FilePath -> Config -> Config
setTemplate fp c = c { cTemplate = Just fp }

setCompiler :: FilePath -> Config -> Config
setCompiler fp c = c { cCompiler = Just fp }

setLinker :: FilePath -> Config -> Config
setLinker fp c = c { cLinker = Just fp }

setKeepFiles :: Bool -> Config -> Config
setKeepFiles b c = c { cKeepFiles = b }

setCrossCompile :: Bool -> Config -> Config
setCrossCompile b c = c { cCrossCompile = b }

setVerbose :: Bool -> Config -> Config
setVerbose v c = c { cVerbose = v }

include :: String -> Flag
include s@('\"':_) = Include s
include s@('<' :_) = Include s
include s          = Include ("\""++s++"\"")

define :: String -> Flag
define s = case break (== '=') s of
    (name, [])      -> Define name Nothing
    (name, _:value) -> Define name (Just value)

