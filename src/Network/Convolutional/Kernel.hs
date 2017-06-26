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
    Kernel :: { _summation :: DualWeights w h d -> Dual Bias -> DualImage w h d -> Dual Output
              , _weights   :: Weights w h d
              , _bias      :: Bias
              } -> Kernel w h d

makeLenses ''Kernel
