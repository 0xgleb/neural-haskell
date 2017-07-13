{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Convolutional.Kernel where

import Data.Vector.Sized
import GHC.TypeLits

import Control.Lens

import AutoDiff
import Network.CommonTypes
import Network.Convolutional.Types

data Kernel w h d where
    Kernel :: (NZ w w', NZ h h', NZ d d') =>
        { summation :: DualWeights w h d -> Dual Bias -> DualImage w h d -> Dual Output
        , bias      :: Bias
        , weights   :: Weights w h d
        } -> Kernel w h d
