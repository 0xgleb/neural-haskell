{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Network.Types where

import Data.Vector.Sized hiding ((++))

import Control.Lens

import AutoDiff

type Number = Double

type Bias          = Number
type Weights     n = Vector Number n
type DualWeights n = Vector (Dual Number) n
type Activations n = Vector Number n
type Input         = Number
type Output        = Number
type Error         = Number
type Iterations    = Integer
data StopCriteria  = StopCriteria { _maxError     :: Error
                                  , _maxIteration :: Iterations
                                  }

makeLenses ''StopCriteria

data ErrorFunction m where
    ErrorFunction :: { unErrF :: Vector (Dual Output) m -> Vector (Dual Output) m -> Dual Number } -> ErrorFunction m

data TotalErrorFunction m o where
    TotalErrorFunction :: { unTotErrF :: Vector (Vector Output o) m -> Vector (Vector Output o) m -> Output } -> TotalErrorFunction m o

data Example i o where
    Example :: { _input  :: Vector Number i
               , _output :: Vector Output o
               } -> Example i o

makeLenses ''Example

type LearningRate = Number
