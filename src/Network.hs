{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Network
( module Network.Neuron
, Network(..)
, nonDiffSum
, initNet
, runNetwork
, doubleMap
, shouldStop
, toNetwork
, getTotalError
, train
, cvTrain
) where

import Prelude hiding ((!!), (++), head, tail, foldl, zipWith, map, sum, length, mapM)

import Data.Vector.Sized
import GHC.TypeLits

import Control.Lens
import Control.Monad (liftM2)

import System.Random

import AutoDiff
import Network.Types
import Network.Neuron


infixr 5 :~~

data Network inputs layers outputs where
    NilNetwork :: (NZ i i') => Network i 0 i
    (:~~)      :: (NZ i i', NZ o o', NZ m m') => Vector i (Neuron m) -> Network i l o -> Network m (l + 1) o


nonDiffSum :: (DualWeights n -> Dual Bias -> Activations n -> Dual Output) -> Weights n -> Bias -> Activations n -> Output
nonDiffSum summ w b i = val $ summ (map constDual w) (constDual b) i

initNet :: Network i l o -> IO (Network i l o)
initNet NilNetwork          = return NilNetwork
initNet (layer :~~ netTail) = liftM2 (:~~) (mapM initNeuron layer) $ initNet netTail

runNetwork :: Network i l o -> Vector i Input -> Vector o Output
runNetwork NilNetwork          inputs = inputs
runNetwork (layer :~~ netTail) inputs = runNetwork netTail $ map (\n -> (toNormalFunc $ n ^. activation) $ (nonDiffSum $ n ^. summation) (n ^. weights) (n ^. bias) inputs) layer


doubleMap :: (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
doubleMap f v1 v2 = map (uncurry f) $ zipWith (,) v1 v2


shouldStop :: Either StopCriteria (Either Error Iterations) -> Error -> Iterations -> Bool
shouldStop (Left criteria)   err iter = (criteria ^. maxError) >= err || (criteria ^. maxIteration) <= iter
shouldStop (Right (Left m))  err _    = m >= err
shouldStop (Right (Right i)) _   iter = i <= iter


toNetwork :: (NZ i i', NZ o o') => Vector o (Neuron i) -> Network i 1 o
toNetwork layer = layer :~~ NilNetwork


getTotalError :: ErrorFunction o -> TotalErrorFunction m o
getTotalError errF = TotalErrorFunction $ \v1 v2 -> val $ sum $ zipWith (unErrF errF) (map (map constDual) v1) (map (map constDual) v2)


dAct :: Vector o (Neuron i) -> Vector i Input -> Vector o Output
dAct layer inputs = map (\n -> d (n ^. activation) $ (nonDiffSum $ n ^. summation) (n ^. weights) (n ^. bias) inputs) layer


updateLayer :: Vector o (Neuron i) -> Vector i Input -> Vector o Error -> LearningRate -> Vector o (Neuron i)
updateLayer layer inputs curErr rate = doubleMap updNeuron layer curErr
    where updNeuron n e = n & weights %~ (\ws -> zipWith (\p n -> p - rate * n) ws $ scaleVector e inputs) & bias %~ (\b -> b - rate * e)


updateNetwork :: (NZ m m', NZ i i') => Vector i (Neuron m) -> Network i l o -> ErrorFunction o -> LearningRate -> Vector m Input -> Vector o Output -> (Vector i Error, Network m (l + 1) o)
updateNetwork layer NilNetwork errF rate inputs expected = (curErr, toNetwork $ updateLayer layer inputs curErr rate)
    where curErr = zipWith (*) (dAct layer inputs) $ grad (unErrF errF $ map constDual expected) $ runNetwork (toNetwork layer) inputs 

updateNetwork layer (nextL :~~ netTail) errF rate inputs expected = (curErr, updateLayer layer inputs curErr rate :~~ snd atNextLayer)
    where curErr = zipWith (*) (dAct layer inputs) $ fst atNextLayer `multiply` map _weights nextL
          multiply :: (NZ n n') => Vector n Error -> Vector n (Weights i) -> Vector i Error
          multiply errors ws = (\v -> foldl (doubleMap (+)) (head v) (tail v)) $ doubleMap scaleVector errors ws
          atNextLayer = updateNetwork nextL netTail errF rate (runNetwork (toNetwork layer) inputs) expected


train :: (NZ l l') => ErrorFunction o -> LearningRate -> Vector m (Example i o) -> Either StopCriteria (Either Error Iterations) -> Network i l o -> Network i l o
train errF rate examples stopCriteria network = trainNetwork network 0
    where trainNetwork net i
            | shouldStop stopCriteria (unTotErrF (getTotalError errF) (map (runNetwork net . (^. input)) examples) $ map (^. output) examples) i = net
            | otherwise = trainNetwork (foldl (\(layer :~~ netTail) e -> snd $ updateNetwork layer netTail errF rate (e ^. input) (e ^. output)) net examples) (i + 1)

cvTrain :: (NZ l l') => ErrorFunction o -> LearningRate -> Vector m (Example i o) -> Vector n (Example i o) -> Either StopCriteria (Either Error Iterations) -> Network i l o -> Network i l o
cvTrain errF rate trainingExamples validationExamples stopCriteria network = trainNetwork network 0
    where trainNetwork net i
            | shouldStop stopCriteria (unTotErrF (getTotalError errF) (map (runNetwork net . (^. input)) validationExamples) $ map (^. output) validationExamples) i = net
            | otherwise = trainNetwork (foldl (\(layer :~~ netTail) e -> snd $ updateNetwork layer netTail errF rate (e ^. input) (e ^. output)) net trainingExamples) (i + 1)
