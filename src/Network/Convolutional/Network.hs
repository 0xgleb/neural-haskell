{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Convolutional.Network where

import Prelude hiding ((++), replicate)
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
    NilNet :: (NZ iw iw', NZ ih ih') => Network w h d w h d
    ActivL :: Activation -> Network iw ih id ow oh od -> Network iw ih id ow oh od
    ConvL  :: Vector d1 (Kernel kw kh d2) -> Proxy s -> Network (w + 1) (h + 1) d1 ow oh od -> Network ((w * s) + kw) ((h * s) + kh) d2 ow oh od
    FCL    :: Vector d1 (Kernel iw ih d2) -> Network 1 1 d1 ow oh od -> Network iw ih d2 ow oh od
    Pad    :: (KnownNat pw, KnownNat ph, KnownNat ih, KnownNat id) => Proxy pw -> Proxy ph -> Network (pw + iw + pw) (ph + ih + ph) id ow oh od -> Network iw ih id ow oh od

runNetwork :: Network iw ih id ow oh od -> Image iw ih id -> Image ow oh od
runNetwork NilNet                  image = image
runNetwork (ActivL f netTail)      image = runNetwork netTail $ fmap (fmap $ fmap $ toNormalF f) image
runNetwork (Pad wPad hPad netTail) image = runNetwork netTail $ zerosW ++ fmap (\v -> zerosH ++ v ++ zerosH) image ++ zerosW
    where zerosW = replicate' wPad $ zerosH ++ replicate (replicate 0) ++ zerosH
          zerosH = replicate' hPad $ replicate 0
