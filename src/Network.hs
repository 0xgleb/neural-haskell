{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE TypeOperators #-}

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

runNetwork :: Network i l o -> Vector Double i -> Vector Double o
runNetwork NilNetwork          inputs = inputs
runNetwork (layer :~~ netTail) inputs = runNetwork netTail $ map (\n -> val $ (n ^. activation) $ (n ^. summation) (map constDual $ n ^. weights) (constDual $ n ^. bias) inputs) layer

type Error        = Double
type Iterations   = Int
data StopCriteria = StopCriteria { _maxError     :: Error
                                 , _maxIteration :: Iterations
                                 }

makeLenses ''StopCriteria

shouldStop :: Either StopCriteria (Either Error Iterations) -> Error -> Iterations -> Bool
shouldStop (Left criteria)   err iter = (criteria ^. maxError) >= err || (criteria ^. maxIteration) <= iter
shouldStop (Right (Left m))  err _    = m >= err
shouldStop (Right (Right i)) _   iter = i <= iter

-- teachNetwork :: ErrorFunction m -> LearningRate -> Vector (Example i) m -> Network i l o -> Either StopCriteria (Either Error Iterations) -> NetowrkArgs i l o -> NetworkArgs i l o
-- teachNetwork err learnRate examples stopCriteria network args = 
