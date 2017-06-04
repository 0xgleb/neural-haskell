{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds, GADTs #-}

module Neuron.Model where

import Control.Lens

import Prelude hiding ((++), head, tail, foldl, zipWith, map, sum)
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

data ErrorFunction m where
    ErrorFunction :: { unErrF :: Vector (Dual Output) m -> Vector (Dual Output) m -> Dual Double } -> ErrorFunction m

standartError :: ErrorFunction n
standartError = ErrorFunction $ \actual expected -> (/2) . foldl (+) 0 $ map (^2) $ zipWith (-) actual expected

runNeuron :: Neuron n -> Vector Double n -> Double
runNeuron neuron = val . (neuron ^. activation) . ((neuron ^. summation $ map constDual $ neuron ^. weights) $ constDual $ neuron ^. bias)

data Example n where
    Example :: { _input  :: Vector Double n
               , _output :: Output
               } -> Example n

makeLenses ''Example

type LearningRate = Double

updateNeuron :: Neuron n -> LearningRate -> (Vector Double (S n) -> Vector Double (S n)) -> Neuron n
updateNeuron neuron learnRate f = let result = scaleVector learnRate $ f $ (neuron ^. bias) :- (neuron ^. weights) in (neuron & bias .~ (head result)) & weights .~ (tail result)

gradientableError :: Neuron n -> ErrorFunction m -> Vector (Example n) m -> (Vector (Dual Double) (S n) -> Dual Double)
gradientableError neuron err examples ws = unErrF err (map (constDual . (^. output)) examples) $ map ((neuron ^. activation) . (neuron ^. summation) (tail ws) (head ws) . (^. input)) examples

teach :: ErrorFunction m -> LearningRate -> Vector (Example n) m -> Neuron n -> Neuron n
teach err learnRate examples neuron = if (abs ((updatedNeuron ^. bias) - (neuron ^. bias)) < (learnRate / 100)) && (modulus (zipWith (-) (updatedNeuron ^. weights) (neuron ^. weights)) < (learnRate / 100)) then neuron else teach err learnRate examples updatedNeuron
    where updatedNeuron = updateNeuron neuron learnRate $ grad $ gradientableError neuron err examples
          modulus = sqrt . sum . map (^2)
