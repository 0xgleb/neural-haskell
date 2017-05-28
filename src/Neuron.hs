{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds, GADTs #-}

module Neuron where

import Control.Lens

import Prelude hiding ((++), head, tail, foldl, zipWith, map)
import Data.Type.Natural
import Data.Vector.Sized
import AutoDiff

type Bias          = Double
type Weights     n = Vector Double n
type DualWeights n = Vector (Dual Double) n
type Activations n = Vector Double n
type Output        = Double

data Neuron n where
    Neuron :: { _summation  :: DualWeights n -> Dual Bias -> Activations n -> Dual Output
              , _activation :: Dual Double -> Dual Output
              , _weights    :: Weights n
              , _bias       :: Bias
              } -> Neuron n

makeLenses ''Neuron

biasedWeights :: Neuron n -> Weights (S n)
biasedWeights neuron = (neuron ^. bias) :- (neuron ^. weights)

-- type ErrorFunction = Vector Double n -> Vector Double n -> Dual Double

standartError :: Vector Output n -> Vector Output n -> Double
standartError expected actual = foldl (+) 0 $ map ((/2) . (^2)) $ zipWith (-) expected actual

runNeuron :: Neuron n -> Vector Double n -> Double
runNeuron neuron = val . (neuron ^. activation) . ((neuron ^. summation $ map constDual $ neuron ^. weights) $ constDual $ neuron ^. bias)

data Example n where
    Example :: { _input  :: Vector Double n
               , _output :: Output
               } -> Example n

makeLenses ''Example

{-
teach :: Neuron -> Double -> Vector Example -> ErrorFunction -> Neuron
teach neuron learnRate examples err = let updated = updateWeights neuron learnRate examples in if fst updated then snd updated else updateWeights (snd updated) learnRate examples
    where updateWeights :: Neuron -> Double -> Examples -> ErrorFunction -> (Bool, Neuron)
          updateWeights neuron learnRate examples err
            | null examples = (True, Neuron)
            | runNeuron neuron (head examples ^. input) == (head examples ^. output) = updateWeights neuron learnRate $ tail examples
            | otherwise = (False, snd $ updateWeights (neuron %~ weights %~ (- (learnRate * grad (err $ head examples )))) learnRate) -}
