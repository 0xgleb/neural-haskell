{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module Network.Convolutional.Pool where

import Data.Vector.Sized
import GHC.TypeLits

import Control.Lens

import Network.Convolutional.Types

type Pool w h n m = forall d. DualImage (n * w) (m * h) d -> DualImage w h d
