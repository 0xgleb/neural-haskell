{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds, GADTs #-}

module Network where

import Prelude hiding ((!!), (++), head, tail, foldl, zipWith, map, sum)

import Data.Type.Natural
import Data.Vector.Sized
import Data.Type.Equality

import Control.Lens

import AutoDiff
import Neuron.Model hiding (Example)

infixr 5 :~~

data Network inputs layers outputs where
    NilNetwork :: i ~ S i' => Network i Z i
    (:~~)      :: (i ~ S i', o ~ S o', m ~ S m') => Vector (Neuron m) i -> Network i l o -> Network m (S l) o

nonDiffSum :: (DualWeights n -> Dual Bias -> Activations n -> Dual Output) -> Weights n -> Bias -> Activations n -> Output
nonDiffSum summ w b i = val $ summ (map constDual w) (constDual b) i

runNetwork :: Network i l o -> Vector Double i -> Vector Double o
runNetwork NilNetwork          inputs = inputs
runNetwork (layer :~~ netTail) inputs = runNetwork netTail $ map (\n -> (toNormalFunc $ n ^. activation) $ (nonDiffSum $ n ^. summation) (n ^. weights) (n ^. bias) inputs) layer

type Error        = Double
type Iterations   = Integer
data StopCriteria = StopCriteria { _maxError     :: Error
                                 , _maxIteration :: Iterations
                                 }

makeLenses ''StopCriteria

doubleMap :: (a -> b -> c) -> Vector a n -> Vector b n -> Vector c n
doubleMap f v1 v2 = map (uncurry f) $ zipWithSame (,) v1 v2

shouldStop :: Either StopCriteria (Either Error Iterations) -> Error -> Iterations -> Bool
shouldStop (Left criteria)   errF iter = (criteria ^. maxError) >= errF || (criteria ^. maxIteration) <= iter
shouldStop (Right (Left m))  errF _    = m >= err
shouldStop (Right (Right i)) _   iter = i <= iter

toNetwork :: (i ~ S i', o ~ S o') => Vector (Neuron i) o -> Network i (S Z) o
toNetwork layer = layer :~~ NilNetwork

dAct :: Vector (Neuron i) o -> Vector Double i -> Vector Double o
dAct layer inputs = map (\n -> d (n ^. activation) $ (nonDiffSum $ n ^. summation) (n ^. weights) (n ^. bias) inputs) layer

updateLayer :: Vector (Neuron i) o -> Vector Double i -> Vector Error o -> LearningRate -> Vector (Neuron i) o
updateLayer layer inputs curErr rate = doubleMap (\n e -> n & weights %~ (\ws -> zipWithSame (\p n -> p - rate * n) ws $ scaleVector e $ zipWithSame (*) inputs ws) & bias %~ (\b -> b - rate * e)) layer curErr

updateNetwork :: (m ~ S m', i ~ S i') => Vector (Neuron m) i -> Network i l o -> ErrorFunction o -> LearningRate -> Vector Double m -> Vector Double o -> (Vector Error i, Network m (S l) o)
updateNetwork layer NilNetwork          errF rate inputs expected = let curErr = zipWithSame (*) (dAct layer inputs) $ grad (flip (unErrF err) $ map constDual expected) $ runNetwork (toNetwork layer) inputs in (curErr, toNetwork $ updateLayer layer inputs curErr rate)
updateNetwork layer (nextL :~~ netTail) errF rate inputs expected = let curErr = zipWithSame (*) (dAct layer inputs) $ fst atNextLayer `multiply` map _weights nextL in (curErr, updateLayer layer inputs curErr rate :~~ snd atNextLayer)
    where multiply :: Vector Error n -> Vector (Weights i) n -> Vector Double i
          multiply errors ws = (\(x :- xs) -> foldl (doubleMap (+)) x xs) $ map (uncurry scaleVector) $ zipWithSame (,) errors ws
          atNextLayer = updateNetwork nextL netTail errF rate (runNetwork (toNetwork layer) inputs) expected

data Example i o where
    Example :: { _input  :: Vector Double i
               , _output :: Vector Output o
               } -> Example i o

makeLenses ''Example

teach :: l ~ S l' => ErrorFunction m -> ErrorFunction o -> LearningRate -> Vector (Example i o) m -> Network i l o -> Either StopCriteria (Either Error Iterations) -> NetowrkArgs i l o -> NetworkArgs i l o
teach totalErr errF learnRate examples stopCriteria network args = teachNetwork network 0
    where teachNetwork :: Network i l o -> Iterations -> Network i l o
          teachNetwork net i
            | not $ shouldStop stopCriteria (networkError net) i = teachNetwork (foldl updateNetworkHelper net examples) (i + 1)
            | otherwise = net

          networkError :: Network i l o -> Error
          networkError = undefined

          updateNetworkHelper :: Network i l o -> Example i o -> Network i l o
          updateNetworkHelper net@(layer :~~ netTail) example = if example ^. output == runNetwork net (examples ^. input) then net else snd $ updateNetwork layer netTail errF (example ^. inputs) (example ^. outputs)
