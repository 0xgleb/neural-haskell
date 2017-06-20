{-# LANGUAGE DataKinds #-}

module Network.Neurons where

import Prelude hiding (sum, zipWith, map)
import GHC.TypeLits

import AutoDiff

import Network.Neuron
import Network.Types

standartSum :: Num a => Vector n (Dual a) -> (Dual a) -> Vector n a -> Dual a
standartSum ws b a = (+ b) $ sum $ zipWith (*) ws $ map constDual a

zeros :: KnownNat n => (Weights n -> Bias -> Neuron n) -> Neuron n
zeros = ($ 0) . ($ generate $ const 0)

sigmoid :: Floating a => a -> a
sigmoid x = 1 / (1 + exp (-x))

newLogisticNeuron :: Vector n Number -> Number -> Neuron n
newLogisticNeuron = Neuron standartSum sigmoid

logisticNeuron :: KnownNat n => Neuron n
logisticNeuron = zeros newLogisticNeuron

newLinearNeuron :: Weights n -> Bias -> Neuron n
newLinearNeuron = Neuron standartSum id

linearNeuron :: KnownNat n => Neuron n
linearNeuron = zeros newLinearNeuron 

newTanhNeuron :: Weights n -> Bias -> Neuron n
newTanhNeuron = Neuron standartSum tanh

tanhNeuron :: KnownNat n => Neuron n
tanhNeuron = zeros newTanhNeuron

relu :: Dual Number -> Dual Number
relu x = if x <= 0 then Dual 0 0 else x

newReluNeuron :: Weights n -> Bias -> Neuron n
newReluNeuron = Neuron standartSum relu

reluNeuron :: KnownNat n => Neuron n
reluNeuron = zeros newReluNeuron

softplus :: Floating a => a -> a
softplus x = log $ 1 + exp x

newSoftplusNeuron :: Weights n -> Bias -> Neuron n
newSoftplusNeuron = Neuron standartSum softplus

softplusNeuron :: KnownNat n => Neuron n
softplusNeuron = zeros newSoftplusNeuron
