{-# LANGUAGE DataKinds #-}

module NeuronSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Type.Natural
import Data.Vector.Sized

import Neuron

weights :: Vector Double (S (S (S Z)))
weights = unsafeFromList' [0, 0, 0]

examples :: Vector (Example (S (S (S Z)))) (S (S (S Z)))
examples = unsafeFromList' [ Example (unsafeFromList' [1, 1  , 0.3]) 1
                           , Example (unsafeFromList' [1, 0.4, 0.5]) 1
                           , Example (unsafeFromList' [1, 0.7, 0.8]) 0
                           ]

spec :: Spec
spec = do
    describe "teach" $ do
        it "can teach a perceptron" $ do


main :: IO ()
main = hspec spec
