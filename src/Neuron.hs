{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds, GADTs #-}

module Neuron where

import Control.Lens

import Prelude hiding ((++), head, tail, foldl, zipWith, map)
import Data.Type.Natural
import Data.Vector.Sized
import AutoDiff

scaleVector :: Num a => a -> Vector a n -> Vector a n
scaleVector x = map (* x)

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
standartError expected actual = (/2) . foldl (+) 0 $ map (^2) $ zipWith (-) expected actual

runNeuron :: Neuron n -> Vector Double n -> Double
runNeuron neuron = val . (neuron ^. activation) . ((neuron ^. summation $ map constDual $ neuron ^. weights) $ constDual $ neuron ^. bias)

data Example n where
    Example :: { _input  :: Vector Double n
               , _output :: Output
               } -> Example n

makeLenses ''Example

type LearnRate = Double

updateNeuron :: Neuron n -> (Vector Double (S n) -> Vector Double (S n)) -> Neuron n
updateNeuron neuron f = let result = f $ (neuron ^. bias) :- (neuron ^. weights) in (neuron & bias .~ (head result)) & weights .~ (tail result)

gradientableError :: Neuron n -> (Vector (Dual Output) m -> Vector (Dual Output) m -> Dual Double) -> Vector (Example n) m -> (Vector (Dual Double) (S n) -> Dual Double)
gradientableError neuron err examples ws = err (map (constDual . (^. output)) examples) $ map ((neuron ^. activation) . (neuron ^. summation) (tail ws) (head ws) . (^. input)) examples

teach :: Neuron n -> LearnRate -> Vector (Example n) m -> (Vector (Dual Output) m -> Vector (Dual Output) m -> Dual Double) -> Neuron n
teach neuron learnRate examples err = if (updatedNeuron ^. bias == neuron ^. bias) && (updatedNeuron ^. weights == neuron ^. weights) then neuron else teach updatedNeuron learnRate examples err
    where updatedNeuron = updateNeuron neuron $ grad $ gradientableError neuron err examples
