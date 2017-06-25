{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Convolutional.Kernel where

import Data.Vector.Sized
import GHC.TypeLits

import Control.Lens

import AutoDiff
import Network.CommonTypes
import Network.Convolutional.Types

data Kernel w h where
    Kernel :: { _summation :: DualWeights w h -> Dual Bias -> DualImage w h -> Dual Output
              , _weights   :: Weights w h
              , _bias      :: Bias
              } -> Kernel w h

makeLenses ''Kernel

-- runKernel :: Image 
