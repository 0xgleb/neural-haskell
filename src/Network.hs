{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network
( module Network.Neuron
, Network(..)
, nonDiffSum
, runNetwork
, doubleMap
, shouldStop
, toNetwork
, getTotalError
, teach
) where

import Prelude hiding ((!!), (++), head, tail, foldl, zipWith, map, sum, length)

import Data.Type.Natural
import Data.Vector.Sized
import Data.Type.Equality

import Control.Lens

import AutoDiff
import Network.Types
import Network.Neuron

infixr 5 :~~

data Network inputs layers outputs where
    NilNetwork :: i ~ S i' => Network i Z i
    (:~~)      :: (i ~ S i', o ~ S o', m ~ S m') => Vector (Neuron m) i -> Network i l o -> Network m (S l) o

nonDiffSum :: (DualWeights n -> Dual Bias -> Activations n -> Dual Output) -> Weights n -> Bias -> Activations n -> Output
nonDiffSum summ w b i = val $ summ (map constDual w) (constDual b) i

runNetwork :: Network i l o -> Vector Input i -> Vector Output o
runNetwork NilNetwork          inputs = inputs
runNetwork (layer :~~ netTail) inputs = runNetwork netTail $ map (\n -> (toNormalFunc $ n ^. activation) $ (nonDiffSum $ n ^. summation) (n ^. weights) (n ^. bias) inputs) layer

doubleMap :: (a -> b -> c) -> Vector a n -> Vector b n -> Vector c n
doubleMap f v1 v2 = map (uncurry f) $ zipWithSame (,) v1 v2

shouldStop :: Either StopCriteria (Either Error Iterations) -> Error -> Iterations -> Bool
shouldStop (Left criteria)   err iter = (criteria ^. maxError) >= err || (criteria ^. maxIteration) <= iter
shouldStop (Right (Left m))  err _    = m >= err
shouldStop (Right (Right i)) _   iter = i <= iter

toNetwork :: (i ~ S i', o ~ S o') => Vector (Neuron i) o -> Network i (S Z) o
toNetwork layer = layer :~~ NilNetwork

getTotalError :: ErrorFunction o -> TotalErrorFunction m o
getTotalError errF = TotalErrorFunction $ \v1 v2 -> val $ sum $ zipWith (unErrF errF) (map (map constDual) v1) (map (map constDual) v2)

dAct :: Vector (Neuron i) o -> Vector Input i -> Vector Output o
dAct layer inputs = map (\n -> d (n ^. activation) $ (nonDiffSum $ n ^. summation) (n ^. weights) (n ^. bias) inputs) layer

updateLayer :: Vector (Neuron i) o -> Vector Input i -> Vector Error o -> LearningRate -> Vector (Neuron i) o
updateLayer layer inputs curErr rate = doubleMap updNeuron layer curErr
    where updNeuron n e = n & weights %~ (\ws -> zipWithSame (\p n -> p - rate * n) ws $ scaleVector e inputs) & bias %~ (\b -> b - rate * e)
-- where updNeuron n e = n & weights %~ (\ws -> zipWithSame (\p n -> p - rate * n) ws $ scaleVector e inputs) & weights %~ trace "updated weights" & bias %~ (\b -> b - rate * e) & bias %~ trace "updates bias"

updateNetwork :: (m ~ S m', i ~ S i') => Vector (Neuron m) i -> Network i l o -> ErrorFunction o -> LearningRate -> Vector Input m -> Vector Output o -> (Vector Error i, Network m (S l) o)
updateNetwork layer NilNetwork errF rate inputs expected = (curErr, toNetwork $ updateLayer layer inputs curErr rate)
    where curErr = zipWithSame (*) (dAct layer inputs) $ grad (unErrF errF $ map constDual expected) $ runNetwork (toNetwork layer) inputs 

updateNetwork layer (nextL :~~ netTail) errF rate inputs expected = (curErr, updateLayer layer inputs curErr rate :~~ snd atNextLayer)
    where curErr = zipWithSame (*) (dAct layer inputs) $ fst atNextLayer `multiply` map _weights nextL
          multiply :: Vector Error n -> Vector (Weights i) n -> Vector Error i
          multiply errors ws = (\(x :- xs) -> foldl (doubleMap (+)) x xs) $ doubleMap scaleVector errors ws
          atNextLayer = updateNetwork nextL netTail errF rate (runNetwork (toNetwork layer) inputs) expected

teach :: l ~ S l' => ErrorFunction o -> LearningRate -> Vector (Example i o) m -> Either StopCriteria (Either Error Iterations) -> Network i l o -> Network i l o
teach errF rate examples stopCriteria network = teachNetwork network 0
    where teachNetwork net i
            | shouldStop stopCriteria (networkError net) i = net
            | otherwise = teachNetwork (foldl (\(layer :~~ netTail) e -> snd $ updateNetwork layer netTail errF rate (e ^. input) (e ^. output)) net examples) (i + 1)

          networkError net = unTotErrF (getTotalError errF) (map (runNetwork net . (^. input)) examples) $ map (^. output) examples
