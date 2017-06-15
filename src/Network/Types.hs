{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}

module Network.Types where

import Data.Vector.Sized hiding ((++))
import GHC.TypeLits

import Control.Lens

import AutoDiff

type NZ x x' = x ~ (x' + 1)

type Number = Double

type Bias          = Number
type Weights     n = Vector n Number
type DualWeights n = Vector n (Dual Number)
type Activations n = Vector n Number
type Input         = Number
type Output        = Number
type Error         = Number
type Iterations    = Integer
data StopCriteria  = StopCriteria { _maxError     :: Error
                                  , _maxIteration :: Iterations
                                  }

makeLenses ''StopCriteria

data ErrorFunction m where
    ErrorFunction :: { unErrF :: Vector m (Dual Output) -> Vector m (Dual Output) -> Dual Number } -> ErrorFunction m

data TotalErrorFunction m o where
    TotalErrorFunction :: { unTotErrF :: Vector m (Vector o Output) -> Vector m (Vector o Output) -> Output } -> TotalErrorFunction m o

data Example i o where
    Example :: { _input  :: Vector i Number
               , _output :: Vector o Output
               } -> Example i o

makeLenses ''Example

type LearningRate = Number
