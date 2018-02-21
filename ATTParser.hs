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

import Data.Functor ((<$>))
import Control.Applicative ((<|>))

type ASM = [(String, [(String, String)])]

parse :: FilePath -> IO ASM
parse f = do
  lns <- lines <$> readFile f
  return $ foldl parseLine [] lns

  where parseLine :: ASM -> String -> ASM
        parseLine [] ('\t':_) = []
        parseLine ((ident,attr):xs) ('\t':line) = let (key, val) = span (`notElem` " \t") line
                                                  in (ident,(key,trim val):attr):xs
        parseLine xs line = let ident = takeWhile (/= ':') line in (ident,[]):xs

trim :: String -> String
trim = reverse . dropWhile (`elem` " \t") . reverse . dropWhile (`elem` " \t")

-- | lookup a constant numeric value. Drop any comments indicated by ';', '#' or '@'.
-- We assume the value is either in the `.long` or `.quad` attribute.
lookupConst :: String -> ASM -> Maybe String
lookupConst key asm = lookup key asm >>= \x -> (trim . takeWhile (`notElem` ";#@") <$> (lookup ".long" x <|> lookup ".quad" x))
                                               -- the compiler may emit something like `.space 4` to indicate 0000.
                                               <|> (const "0" <$> lookup ".space" x)

-- | extract a C String in the most basic sense we can.
-- the .asciz directive doesn't contain the \0 terminator.
lookupASCII :: String -> ASM -> Maybe String
lookupASCII key asm = lookup key asm >>= \x -> read <$> lookup ".ascii" x <|> ((++ "\0") . read <$> lookup ".asciz" x)

lookupInt :: String -> ASM -> Maybe Int
lookupInt key = fmap read . lookupConst key

lookupInteger :: String -> ASM -> Maybe Integer
lookupInteger key = fmap read . lookupConst key

lookupUInteger :: String -> ASM -> Maybe Integer
lookupUInteger key = fmap (fromIntegral . (read :: String -> Word)) . lookupConst key

lookupCString :: String -> ASM -> Maybe String
lookupCString key asm = lookupConst key asm >>= flip lookupASCII asm
