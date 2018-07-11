module Network.Classic.Types where

import           AutoDiff
import           Network.CommonTypes

import           Data.Vector.Sized
import           GHC.TypeLits

import           Control.Lens

type Weights     n = Vector n Number
type DualWeights n = Vector n (Dual Number)
type Activations n = Vector n Number
