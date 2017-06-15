{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Network.Neuron
( Neuron(..)
, summation
, activation
, weights
, bias
, scaleVector
, biasedWeights
, squareError
, runNeuron
) where

import Control.Lens hiding (cons)

import Prelude hiding ((++), head, tail, foldl, zipWith, map, sum)
import Data.Vector.Sized
import GHC.TypeLits
import AutoDiff

import Network.Types

data Neuron n where
    Neuron :: { _summation  :: DualWeights n -> Dual Bias -> Activations n -> Dual Output
              , _activation :: Dual Output -> Dual Output
              , _weights    :: Weights n
              , _bias       :: Bias
              } -> Neuron n

makeLenses ''Neuron

scaleVector :: Num a => a -> Vector n a -> Vector n a
scaleVector x = map (* x)

biasedWeights :: Neuron n -> Weights (n + 1)
biasedWeights neuron = (neuron ^. bias) `cons` (neuron ^. weights)

squareError :: ErrorFunction n
squareError = ErrorFunction $ \actual expected -> (/2) $ sum $ map (^2) $ zipWith (-) actual expected

runNeuron :: Neuron n -> Vector n Input -> Output
runNeuron neuron = val . (neuron ^. activation) . ((neuron ^. summation $ map constDual $ neuron ^. weights) $ constDual $ neuron ^. bias)

{-
updateNeuron :: Neuron n -> LearningRate -> (Vector Double (S n) -> Vector Double (S n)) -> Neuron n
updateNeuron neuron learnRate f = let result = scaleVector learnRate $ f $ (neuron ^. bias) :- (neuron ^. weights) in (neuron & bias .~ (head result)) & weights .~ (tail result)

gradientableError :: Neuron n -> ErrorFunction m -> Vector (Example n) m -> (Vector (Dual Double) (S n) -> Dual Double)
gradientableError neuron err examples ws = unErrF err (map (constDual . (^. output)) examples) $ map ((neuron ^. activation) . (neuron ^. summation) (tail ws) (head ws) . (^. input)) examples

teach :: ErrorFunction m -> LearningRate -> Vector (Example n) m -> Neuron n -> Neuron n
teach err learnRate examples neuron = if (abs ((updatedNeuron ^. bias) - (neuron ^. bias)) < (learnRate / 100)) && (modulus (zipWith (-) (updatedNeuron ^. weights) (neuron ^. weights)) < (learnRate / 100)) then neuron else teach err learnRate examples updatedNeuron
    where updatedNeuron = updateNeuron neuron learnRate $ grad $ gradientableError neuron err examples
          modulus = sqrt . sum . map (^2)
-}
