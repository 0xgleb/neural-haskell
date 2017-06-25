{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}

module Network.CommonTypes where

import Data.Vector.Sized hiding ((++))
import GHC.TypeLits

import Control.Lens

import AutoDiff

type NZ x x' = x ~ (x' + 1)

type Number = Double

type Bias          = Number
type Input         = Number
type Output        = Number

type Loss          = Number
type Iterations    = Integer
type LearningRate  = Number
data StopCriteria  = StopCriteria { _maxLoss      :: Loss
                                  , _maxIteration :: Iterations
                                  }

makeLenses ''StopCriteria


data LossFunction m where
    LossFunction :: { unLossF :: Vector m (Dual Output) -> Vector m (Dual Output) -> Dual Number } -> LossFunction m


data TotalLossFunction m o where
    TotalLossFunction :: { unTotLossF :: Vector m (Vector o Output) -> Vector m (Vector o Output) -> Output } -> TotalLossFunction m o


data Example i o where
    Example :: { _input  :: Vector i Number
               , _output :: Vector o Output
               } -> Example i o

makeLenses ''Example
