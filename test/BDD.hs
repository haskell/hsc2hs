{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
module BDD where

import Control.Monad (ap)
import Test.Tasty
import Test.Tasty.HUnit

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative (..))
#endif

#if MIN_VERSION_base(4,9,0)
import GHC.Stack (HasCallStack)
#define HASCALLSTACK , HasCallStack
#else
#define HASCALLSTACK
#endif

-------------------------------------------------------------------------------
-- HSpec like DSL for test-framework
-------------------------------------------------------------------------------

specMain :: TestM () -> IO ()
specMain t = runTestM t >>= defaultMain . testGroup "specs"

newtype TestM a = TestM { unTestM :: [TestTree] -> IO ([TestTree], a) }
  deriving (Functor)

-- accumulating in reverse order.
tell1 :: TestTree -> TestM ()
tell1 t = TestM $ \ts -> return (t : ts, ())

instance Applicative TestM where
    pure = return
    (<*>) = ap

instance Monad TestM where
    return x = TestM $ \xs -> return (xs, x)

    m >>= k = TestM $ \xs -> do
        (ys, x) <- unTestM m xs
        unTestM (k x) ys

runTestM :: TestM () -> IO [TestTree]
runTestM (TestM m) = fmap (reverse . fst) (m [])

runIO :: IO a -> TestM a
runIO m = TestM $ \ts -> do
    x <- m
    return (ts, x)

-------------------------------------------------------------------------------
-- describe, it
-------------------------------------------------------------------------------

describe :: TestName -> TestM () -> TestM ()
describe n t =  do
    t' <- runIO (runTestM t)
    tell1 $ testGroup n t'

it :: TestName -> Assertion -> TestM ()
it n assertion = tell1 $ testCase n assertion

shouldBe :: (Eq a, Show a HASCALLSTACK) => a -> a -> Assertion
shouldBe = (@?=)
