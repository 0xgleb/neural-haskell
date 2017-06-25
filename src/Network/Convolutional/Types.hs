{-# LANGUAGE DataKinds, TypeOperators #-}

module Network.Convolutional.Types where

import Network.CommonTypes
import AutoDiff

import Data.Vector.Sized
import GHC.TypeLits

type Image       w h = Vector w (Vector h Number)
type DualImage   w h = Vector w (Vector h (Dual Number))
type Weights     w h = Vector w (Vector h Number)
type DualWeights w h = Vector w (Vector h (Dual Number))
