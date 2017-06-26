{-# LANGUAGE DataKinds #-}

module Network.Neurons where

import Prelude hiding (sum, zipWith, map)
import GHC.TypeLits

import AutoDiff

import Network.Neuron
import Network.Types

standartSum :: Num a => Vector n (Dual a) -> (Dual a) -> Vector n a -> Dual a
standartSum ws b a = (+ b) $ sum $ zipWith (*) ws $ map constDual a

sigmoid :: Floating a => a -> a
sigmoid x = 1 / (1 + exp (-x))

initLogisticNeuron :: Vector n Number -> Number -> Neuron n
initLogisticNeuron = Neuron standartSum sigmoid

logisticNeuron :: KnownNat n => Neuron n
logisticNeuron = Neuron standartSum sigmoid (generate (const 0)) 0
