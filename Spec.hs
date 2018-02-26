module Main where

import Test.Tasty.Hspec
import ATTParser
import Control.Monad (forM_)

main :: IO ()
main = hspec $ do
  describe "asm parser" $ do
    forM_ [("x86_64 linux", "test/asm/x86_64-linux.s")
          ,("x86_64 macos", "test/asm/x86_64-mac.s")
          ,("x86_64 mingw", "test/asm/x86_64-mingw32.s")
          ,("aarch64 ios",  "test/asm/aarch64-ios.s")
          ,("aarch64 linux","test/asm/aarch64.s")]
      $ \(d, f) ->do
      context d $ do
        x <- runIO $ parse f

        it "x should be 1" $ do
          lookupInteger "x" x `shouldBe` (Just 1)
        it "z should be 0xffffffffffffffff" $ do
          lookupInteger "y" x `shouldBe` (Just 0xffffffffffffffff)
        it "z should be -1" $ do
          lookupInteger "z" x `shouldBe` (Just (-1))

        it "t should be \"Hello World\\\"\\n\\0\"" $ do
          lookupString "t" x `shouldBe` (Just "Hello World\" 12345\0")

    forM_ [("arm ios",      "test/asm/arm-ios.s")
          ,("arm linux",    "test/asm/arm.s")
          ,("x86 linux",    "test/asm/x86-linux.s")]
      $ \(d, f) ->do
      context d $ do
        x <- runIO $ parse f

        it "x should be 1" $ do
          lookupInteger "x" x `shouldBe` (Just 1)
        it "z should be 0xffffffff" $ do
          lookupInteger "y" x `shouldBe` (Just 0xffffffff)
        it "z should be -1" $ do
          lookupInteger "z" x `shouldBe` (Just (-1))

        it "t should be \"Hello World\\\"\\n\\0\"" $ do
          lookupString "t" x `shouldBe` (Just "Hello World\" 12345\0")


