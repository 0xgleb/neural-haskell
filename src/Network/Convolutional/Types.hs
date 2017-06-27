{-# LANGUAGE DataKinds #-}

module Network.Convolutional.Types where

import Network.CommonTypes
import AutoDiff

import Data.Vector.Sized
import GHC.TypeLits

type Weights w h = Vector w (Vector h Number)
type DualWeights w h = Vector w (Vector h (Dual Number))
