-- A rather crude asm parser.
--
--
-- we only handle a subset of AT&T assembly
-- right now.  This is what gcc and clang can
-- emit.  For clang using llvm-ir might be
-- even better.  For gcc gimple if that can
-- be consumed reliably somehow.
--
-- For now we'll rely on the at&t assembly
-- to be sufficient for constants.
--


module ATTParser where

import Control.Applicative ((<|>))
import Data.Word (Word32, Word64)
import Data.Int (Int64)
import Data.Char (isDigit, isSpace)
import Data.Bits (shiftL, shiftR, (.|.))
import Data.Maybe (fromMaybe)

data Inst = Ident String
          | Long Word32
          | Quad Word64
          | Ref String
          | Ascii String
          | SectionChange
          deriving Show

mkLong :: Word32 -> Inst
mkLong = Long
mkQuad :: Word64 -> Inst
mkQuad = Quad
-- | turn @x@ and @(x)@ into @Ref x@.
-- The (x) syntax can be found in mips assembly.
mkRef :: String -> Inst
mkRef ('(':r) | (')':r') <- reverse r = Ref $ reverse r'
mkRef r = Ref r

mkAscii :: String -> Inst
mkAscii = Ascii

type ASM = [(String, Inst)]

isIdent :: Inst -> Bool
isIdent (Ident _) = True
isIdent _ = False

-- | Directives that switch the section the assembler writes to. A label's
-- value lives in one section, so such a directive ends the value the way a
-- new label does.
sectionDirectives :: [String]
sectionDirectives = [".section", ".pushsection", ".popsection"]

trim :: String -> String
trim = reverse . dropWhile (`elem` " \t") . reverse . dropWhile (`elem` " \t")
-- | generalized @words@.
words' :: (a -> Bool) -> [a] -> [[a]]
words' p s = case dropWhile p s of
             [] -> []
             s' -> w : words' p s''
                   where (w, s'') = break p s'

isNumber :: String -> Bool
isNumber ('-':x) = all isDigit x
isNumber ('+':x) = all isDigit x
isNumber x       = all isDigit x

-- | process the assembly instructions, filtering out
-- identifiers and constant values.
preprocess :: String -> [Inst]
preprocess [] = []
preprocess ('\t':attr) = let (h, t) = break isSpace attr
                         in case h:words' (=='\t') t of
                         -- 8 byte values
                         (".quad":x:_) | isNumber (w x) -> [mkQuad $ read (w x)]
                                       | otherwise      -> [mkRef  $ (w x)]
                         (".xword":x:_)| isNumber (w x) -> [mkQuad $ read (w x)]
                                       | otherwise      -> [mkRef  $ (w x)]
                         (".8byte":x:_)| isNumber (w x) -> [mkQuad $ read (w x)]
                                       | otherwise      -> [mkRef  $ (w x)]
                         ("data8":x:_) | isNumber (w x) -> [mkQuad $ read (w x)]
                                       | otherwise      -> [mkRef  $ (w x)]

                         -- 4 byte values
                         (".long":x:_) | isNumber (w x) -> [mkLong $ read (w x)]
                                       | otherwise      -> [mkRef  $ (w x)]
                         (".word":x:_) | isNumber (w x) -> [mkLong $ read (w x)]
                                       | otherwise      -> [mkRef  $ (w x)]
                         (".4byte":x:_)| isNumber (w x) -> [mkLong $ read (w x)]
                                       | otherwise      -> [mkRef  $ (w x)]

                         (".space":x:_)| (w x) == "4"   -> [mkLong 0]
                                       | (w x) == "8"   -> [mkQuad 0]
                         (".skip":x:_) | (w x) == "4"   -> [mkLong 0]
                                       | (w x) == "8"   -> [mkQuad 0]

                         (".ascii":x:_)             -> mkAsciiSafe x False
                         (".asciz":x:_)             -> mkAsciiSafe x True
                         -- found on nios, sh4, alpha, mk68k; all without \0.
                         (".string":x:_)            -> mkAsciiSafe x True
                         -- found on hppa
                         (".stringz":x:_)           -> mkAsciiSafe x True
                         -- ia64
                         ("stringz":x:_)            -> mkAsciiSafe x True
                         -- a switch to another section: end the value in
                         -- progress (see 'parseInsts')
                         (d:_) | d `elem` sectionDirectives -> [SectionChange]
                         _                          -> []
  where
    -- Read a C string literal. Metadata sections carry strings that are not
    -- plain Haskell-readable literals (a CodeView record names the source
    -- file, backslashes and all), so a string that will not parse is skipped
    -- instead of crashing the parse.
    mkAsciiSafe x nul = case reads x of
      [(s, "")] -> [mkAscii (if nul then s ++ "\0" else s)]
      _         -> []
    w x = case words x of
      []     -> error $ "preprocess: expected some non-space characters, "
                     ++ "but got spaces only in '" ++ x ++ "'"
      hd : _ -> hd
preprocess ('.':z:'e':'r':'o':'f':'i':'l':'l':' ':x) = case words' (==',') x of
      (_seg:_sect:sym:size:_) | size == "4" -> [Ident sym, mkLong 0]
                              | size == "8" -> [Ident sym, mkQuad 0]
      _ -> []
preprocess ('.':rest) = case words ('.':rest) of
    -- the same section switches, not indented
    (d:_) | d `elem` sectionDirectives -> [SectionChange]
    -- every other directive at the start of a line is a boundary, like a label
    _ -> [Ident $ takeWhile (/= ':') ('.':rest)]
preprocess (c:cs) | not (isSpace c) = [Ident $ takeWhile (/= ':') (c:cs)]
                  | otherwise       = []

-- | turn the list of instructions into an associated list
--
-- A label's value is the constant instructions between the label and the next
-- label. A 'SectionChange' ends that value just like a label does: whatever
-- follows belongs to the new section, which for the parser's purposes is the
-- assembler's debug or metadata output, not a constant. In particular, a
-- section switched to directly after a value (clang's CodeView @.debug$S@
-- records, emitted even at @-g0@) must not be read as a continuation of the
-- value before it, and the instructions before the next label in the new
-- section belong to no label at all.
parseInsts :: [Inst] -> [(String, Inst)]
parseInsts = go Nothing []
  where
    go (Just name) is [] | null is   = []
                         | otherwise = [(name, combineInst is)]
    go _ _ [] = []
    go (Just name) is (SectionChange:xs)
      | null is   = go Nothing [] xs
      | otherwise = (name, combineInst is) : go Nothing [] xs
    go Nothing _ (SectionChange:xs) = go Nothing [] xs
    go (Just name) [] (Ident next:xs) = go (Just next) [] xs
    go (Just name) is (Ident next:xs) =
        (name, combineInst is) : go (Just next) [] xs
    go Nothing _ (Ident next:xs) = go (Just next) [] xs
    go cur is (i:xs) = go cur (is ++ [i]) xs

-- | combine instructions (e.g. two long into a quad)
combineInst :: [Inst] -> Inst
combineInst [Quad i] = Quad i
combineInst [Long i] = Quad (fromIntegral i)
combineInst [Long h, Long l] = Quad $ (shiftL (fromIntegral h) 32) .|. fromIntegral l
combineInst [Ref s]  = Ref s
combineInst [Ascii s] = Ascii s
combineInst is = error $ "Cannot combine instructions: " ++ show is

-- | inline references
inlineRef :: [(String, Inst)] -> [(String, Inst)]
inlineRef xs = map go xs
  where go (k, Ref name) = (k, fromMaybe (error $ "failed to find reference " ++ show name) $ lookup name xs)
        go x = x

fixWordOrder :: [(String, Inst)] -> [(String, Inst)]
fixWordOrder xs = case lookupInteger "___hsc2hs_BOM___" xs of
  Just 1 -> map go xs
  _ -> xs
  where go (k, Quad w) = (k, Quad $ shiftL w 32 .|. shiftR w 32)
        go x = x

parse :: FilePath -> IO [(String, Inst)]
parse f = (fixWordOrder . inlineRef . parseInsts . concatMap preprocess . lines) `fmap` readFile f

-- | lookup a symbol without or with underscore prefix
lookup_ :: String -> [(String,b)] -> Maybe b
lookup_ k l = lookup k l <|> lookup ("_" ++ k) l

lookupString :: String -> [(String, Inst)] -> Maybe String
lookupString k l = case (lookup_ k l) of
  Just (Ascii s) -> Just s
  _ -> Nothing

lookupInteger :: String -> [(String, Inst)] -> Maybe Integer
lookupInteger k l = case (lookup_ k l, lookup_ (k ++ "___hsc2hs_sign___") l) of
  (Just (Quad i), Just (Quad 1)) -> Just (fromIntegral (fromIntegral i :: Int64))
  (Just (Quad i), _) -> Just (fromIntegral i)
  _ -> Nothing
