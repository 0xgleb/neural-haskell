{-# LANGUAGE StandaloneDeriving, DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}

module Network.Convolutional.NetworkSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Prelude hiding (map, zipWith, head, last, (++))
import GHC.TypeLits
import Data.Proxy
import Data.Vector.Sized

import AutoDiff
import Network.Convolutional.Network

regularImage :: Vector 5 (Vector 5 Int)
regularImage = cons (cons 11 $ cons 12 $ cons 13 $ cons 14 $ cons 15 $ empty)
             $ cons (cons 21 $ cons 22 $ cons 23 $ cons 24 $ cons 25 $ empty)
             $ cons (cons 31 $ cons 32 $ cons 33 $ cons 34 $ cons 35 $ empty)
             $ cons (cons 41 $ cons 42 $ cons 43 $ cons 44 $ cons 45 $ empty)
             $ cons (cons 51 $ cons 52 $ cons 53 $ cons 54 $ cons 55 $ empty)
             $ empty

proxy1 :: Proxy 1
proxy1 = Proxy

proxy2 :: Proxy 2
proxy2 = Proxy

spec :: Spec
spec = do
    describe "unsafeTransposeV" $ do
        it "holds on property unsafeTransposeV . unsafeTransposeV == id" $ do
            unsafeTransposeV (unsafeTransposeV regularImage) `shouldBe` regularImage
    describe "unsafeSplitVector" $ do
        it "holds on property unsafeSplitVector proxy1 proxy1 == map singleton, where proxy1 :: Proxy 1" $ do
            unsafeSplitVector proxy1 proxy1 regularImage `shouldBe` map singleton regularImage
            unsafeSplitVector proxy1 proxy1 (head regularImage) `shouldBe` map singleton (head regularImage)
            unsafeSplitVector proxy1 proxy1 (last regularImage) `shouldBe` map singleton (last regularImage)
        it "proxy2 proxy1 [1, 2, 3, 4, 5] == [[1, 2], [2, 3], [3, 4], [4, 5]], where proxy1 :: Proxy 1, proxy2 :: Proxy 2 and [..] are vectors" $ do
            unsafeSplitVector proxy2 proxy1 (head regularImage) `shouldBe` cons (cons 11 $ singleton 12) (cons (cons 12 $ singleton 13) $ cons (cons 13 $ singleton 14) $ singleton $ cons 14 $ singleton 15)

main :: IO ()
main = hspec spec
