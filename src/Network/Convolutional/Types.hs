{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Network.Convolutional.Types where

import           AutoDiff
import           Network.CommonTypes

import           Data.Vector.Sized
import           GHC.TypeLits

type Image       w h d = Vector d (Vector h (Vector w Number))
type DualImage   w h d = Vector d (Vector h (Vector w (Dual Number)))
type Weights     w h d = Vector d (Vector h (Vector w Number))
type DualWeights w h d = Vector d (Vector h (Vector w (Dual Number)))
