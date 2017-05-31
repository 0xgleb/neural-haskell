{-# LANGUAGE DataKinds #-}

module Neuron.Perceptron 
( perceptron
, teached
) where

import Prelude hiding (sum, zipWith, map)
import Data.Vector.Sized
import Data.Type.Natural

import AutoDiff
import Neuron.Model

ws :: Vector Double (S (S (S Z)))
ws = unsafeFromList' [0, 1, 2]

examples :: Vector (Example (S (S (S Z)))) (S (S (S (S Z))))
examples = unsafeFromList' [ Example (unsafeFromList' [0, 0, 1]) 0
                           , Example (unsafeFromList' [0, 1, 1]) 1
                           , Example (unsafeFromList' [1, 0, 1]) 1
                           , Example (unsafeFromList' [1, 1, 1]) 0
                           ]

perceptron :: Weights n -> Bias -> Neuron n
perceptron = Neuron summf activ
    where summf w b x = (+ b) $ sum $ zipWith (*) w $ map constDual x
          activ = id

teached :: Neuron (S (S (S Z)))
teached = teach standartError 1 examples $ perceptron ws 1
