{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Convolutional.Network where

import Data.Vector.Sized
import GHC.TypeLits
import Data.Proxy

import Control.Lens

import AutoDiff
import Network.CommonTypes
import Network.Convolutional.Types
import Network.Convolutional.Pool
import Network.Convolutional.Kernel
import Network.Convolutional.Activation


data Network iw ih id ow oh od where
    NilNet :: (NZ iw iw', NZ ih ih') => Network iw ih 1 iw ih 1
    ActivL :: Activation -> Network iw ih id ow oh od -> Network iw ih id ow oh od
    ConvL  :: Vector k (Kernel kw kh) -> Proxy s -> Network (w + 1) (h + 1) k ow oh od -> Network ((w * s) + kw) ((h * s) + kh) d ow oh od
    Pad    :: Proxy pw -> Proxy ph -> Network (iw + (2 * pw)) (ih + (2 * ph)) id ow oh od -> Network iw ih id ow oh od
