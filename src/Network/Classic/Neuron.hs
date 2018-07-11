{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Network.Classic.Neuron
( Neuron(..)
, module Data.Vector.Sized
, summation
, activation
, weights
, bias
, initNeuron
, scaleVector
, quadraticLoss
, runNeuron
) where

import           Control.Lens          hiding (cons)
import           Data.Vector.Sized
import           GHC.TypeLits
import           Prelude               hiding (foldl, head, mapM, sum, tail,
                                        zipWith, (++))
import           System.Random         (randomRIO)

import           AutoDiff
import           Network.Classic.Types
import           Network.CommonTypes


data Neuron n where
    Neuron :: { _summation  ::
                    DualWeights n -> Dual Bias -> Activations n -> Dual Output
              , _activation :: Dual Output -> Dual Output
              , _weights    :: Weights n
              , _bias       :: Bias
              } -> Neuron n

makeLenses ''Neuron


initNeuron :: Neuron n -> IO (Neuron n)
initNeuron (Neuron s a ws b) =
    Neuron s a <$> mapM (const $ randomRIO (0, 1)) ws <*> randomRIO (0, 1)

scaleVector :: Num a => a -> Vector n a -> Vector n a
scaleVector x = fmap (* x)

quadraticLoss :: LossFunction n
quadraticLoss =
    LossFunction $ \actual expected ->
                     (/2) $ sum $ (^2) <$> zipWith (-) actual expected

runNeuron :: Neuron n -> Vector n Input -> Dual Output
runNeuron neuron = (neuron ^. activation)
                 . ( (neuron ^. summation $ constDual <$> neuron ^. weights)
                   $ constDual $ neuron ^. bias
                   )
