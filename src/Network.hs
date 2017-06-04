{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds, GADTs #-}

module Network where

import Prelude hiding ((!!), (++), head, tail, foldl, zipWith, map, sum)

import Data.Type.Natural
import Data.Vector.Sized
import Data.Type.Equality

import Control.Lens

import AutoDiff
import Neuron.Model

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
type Iterations   = Int
data StopCriteria = StopCriteria { _maxError     :: Error
                                 , _maxIteration :: Iterations
                                 }

makeLenses ''StopCriteria

doubleMap :: (a -> b -> c) -> Vector a n -> Vector b n -> Vector c n
doubleMap f v1 v2 = map (uncurry f) $ zipWithSame (,) v1 v2

shouldStop :: Either StopCriteria (Either Error Iterations) -> Error -> Iterations -> Bool
shouldStop (Left criteria)   err iter = (criteria ^. maxError) >= err || (criteria ^. maxIteration) <= iter
shouldStop (Right (Left m))  err _    = m >= err
shouldStop (Right (Right i)) _   iter = i <= iter

errGradByActivations :: ErrorFunction n -> Vector Output n -> Vector Output n -> Vector Error n
errGradByActivations err expected actual = grad (flip (unErrF err) $ map constDual expected) actual

errAtFirstLayer :: (m ~ S m', i ~ S i') => Vector (Neuron m) i -> Network i l o -> ErrorFunction o -> Vector Double m -> Vector Double o -> Vector Error i
errAtFirstLayer layer NilNetwork          err inputs expected = zipWithSame (*) (errGradByActivations err expected $ runNetwork (layer :~~ NilNetwork) inputs) $ map (\n -> d (n ^. activation) $ (nonDiffSum $ n ^. summation) (n ^. weights) (n ^. bias) inputs) layer
errAtFirstLayer layer (nextL :~~ netTail) err inputs expected = zipWithSame (*) (errAtFirstLayer nextL netTail err (runNetwork (layer :~~ NilNetwork) inputs) expected `multiply` map _weights nextL) $ map (\n -> d (n ^. activation) $ (nonDiffSum $ n ^. summation) (n ^. weights) (n ^. bias) inputs) layer
    where multiply :: Vector Error n -> Vector (Weights i) n -> Vector Double i
          multiply errors ws = (\(x :- xs) -> foldl (doubleMap (+)) x xs) $ map (uncurry scaleVector) $ zipWithSame (,) errors ws

-- teachNetwork :: ErrorFunction m -> LearningRate -> Vector (Example i) m -> Network i l o -> Either StopCriteria (Either Error Iterations) -> NetowrkArgs i l o -> NetworkArgs i l o
-- teachNetwork err learnRate examples stopCriteria network args = 
