{-# LANGUAGE StandaloneDeriving, DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}

-- module Network.Convolutional.NetworkSpec (main, spec) where
module Network.Convolutional.NetworkSpec where

import Test.Hspec
import Test.QuickCheck

import Prelude hiding (sum, map, zipWith, head, last, (++), replicate, maximum)
import GHC.TypeLits
import Data.Proxy
import Data.Vector.Sized

import AutoDiff
import Network.CommonTypes
import Network.Convolutional.Types
import Network.Convolutional.Pool
import Network.Convolutional.Kernel
import Network.Convolutional.Network

regularImage :: Vector 5 (Vector 5 Number)
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

sampleInput :: Image 5 5 3
sampleInput =
    cons ( cons (cons 111 $ cons 112 $ cons 113 $ cons 114 $ cons 115 empty)
         $ cons (cons 121 $ cons 122 $ cons 123 $ cons 124 $ cons 125 empty)
         $ cons (cons 131 $ cons 132 $ cons 133 $ cons 134 $ cons 135 empty)
         $ cons (cons 141 $ cons 142 $ cons 143 $ cons 144 $ cons 145 empty)
         $ cons (cons 151 $ cons 152 $ cons 153 $ cons 154 $ cons 155 empty)
         $ empty
         )
  $ cons ( cons (cons 211 $ cons 212 $ cons 213 $ cons 214 $ cons 215 empty)
         $ cons (cons 221 $ cons 222 $ cons 223 $ cons 224 $ cons 225 empty)
         $ cons (cons 231 $ cons 232 $ cons 233 $ cons 234 $ cons 235 empty)
         $ cons (cons 241 $ cons 242 $ cons 243 $ cons 244 $ cons 245 empty)
         $ cons (cons 251 $ cons 252 $ cons 253 $ cons 254 $ cons 255 empty)
         $ empty
         )
  $ cons ( cons (cons 311 $ cons 312 $ cons 313 $ cons 314 $ cons 315 empty)
         $ cons (cons 321 $ cons 322 $ cons 323 $ cons 324 $ cons 325 empty)
         $ cons (cons 331 $ cons 332 $ cons 333 $ cons 334 $ cons 335 empty)
         $ cons (cons 341 $ cons 342 $ cons 343 $ cons 344 $ cons 345 empty)
         $ cons (cons 351 $ cons 352 $ cons 353 $ cons 354 $ cons 355 empty)
         $ empty
         )
  $ empty

myMax :: Pool 3 3
myMax = maximum . fmap maximum

sampleAfterPooling :: Image 2 2 3
sampleAfterPooling =
    cons ( cons (cons 133 $ cons 135 empty)
         $ cons (cons 153 $ cons 155 empty)
         $ empty
         )
  $ cons ( cons (cons 233 $ cons 235 empty)
         $ cons (cons 253 $ cons 255 empty)
         $ empty
         )
  $ cons ( cons (cons 333 $ cons 335 empty)
         $ cons (cons 353 $ cons 355 empty)
         $ empty
         )
  $ empty

kernel :: Kernel 2 2 3
kernel = Kernel { summation = \ws b i -> (+ b) $ sum $ fmap (sum . fmap sum) $ zipWith (zipWith $ zipWith (*)) ws i
                , weights   = replicate $ replicate $ replicate 1
                , bias      = 0
                }

sampleAfterConv :: Image 4 4 1
sampleAfterConv =
    cons ( cons (cons 2598 $ cons 2610 $ cons 2622 $ cons 2634 empty)
         $ cons (cons 2718 $ cons 2730 $ cons 2742 $ cons 2754 empty)
         $ cons (cons 2838 $ cons 2850 $ cons 2862 $ cons 2874 empty)
         $ cons (cons 2958 $ cons 2970 $ cons 2982 $ cons 2994 empty)
         $ empty
         ) empty

spec :: Spec
spec = do
    describe "unsafeTransposeV" $ do
        it "holds on property unsafeTransposeV . unsafeTransposeV == id" $ do
            unsafeTransposeV (unsafeTransposeV regularImage) `shouldBe` regularImage
    describe "unsafeSplitVector" $ do
        it "proxy2 proxy1 [1, 2, 3, 4, 5] == [[1, 2], [2, 3], [3, 4], [4, 5]], where proxy1 :: Proxy 1, proxy2 :: Proxy 2 and [..] are vectors" $ do
            unsafeSplitVector proxy2 proxy1 (head regularImage) `shouldBe` (cons (cons 11 $ singleton 12) $ cons (cons 12 $ singleton 13) $ cons (cons 13 $ singleton 14) $ singleton $ cons 14 $ singleton 15)
        it "holds on property unsafeSplitVector proxy1 proxy1 == map singleton, where proxy1 :: Proxy 1" $ do
            unsafeSplitVector proxy1 proxy1 regularImage `shouldBe` map singleton regularImage
            unsafeSplitVector proxy1 proxy1 (head regularImage) `shouldBe` map singleton (head regularImage)
            unsafeSplitVector proxy1 proxy1 (last regularImage) `shouldBe` map singleton (last regularImage)
    describe "runNetwork" $ do
        it "holds on property runNetwork NilNet == id" $ do
            runNetwork NilNet (singleton regularImage) `shouldBe` singleton regularImage
            runNetwork NilNet sampleInput `shouldBe` sampleInput
        it "works with an activation layer" $ do
            runNetwork (ActivL ((+1) . (*2)) NilNet) sampleInput `shouldBe` fmap (fmap $ fmap $ toNormalF ((+1) . (*2))) sampleInput
        it "can apply padding to an image" $ do
            runNetwork (Pad proxy1 proxy1 NilNet) sampleInput `shouldBe` fmap (\vec -> singleton (replicate' (Proxy :: Proxy 7) 0) ++ fmap (\v -> cons 0 $ snoc v 0) vec ++ singleton (replicate' (Proxy :: Proxy 7) 0)) sampleInput
        it "works with pooling layers" $ do
            runNetwork (PoolL myMax proxy2 proxy2 NilNet) sampleInput `shouldBe` sampleAfterPooling
        it "works with convolutional layers" $ do
            runNetwork (ConvL (singleton kernel) proxy1 proxy1 NilNet) sampleInput `shouldBe` sampleAfterConv

main :: IO ()
main = hspec spec
