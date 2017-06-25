{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Convolutional.Pool where

import Data.Vector.Sized
import GHC.TypeLits

import Control.Lens

import Network.Convolutional.Types

type Pool w h n m = DualImage (n * w) (m * h) -> DualImage w h
