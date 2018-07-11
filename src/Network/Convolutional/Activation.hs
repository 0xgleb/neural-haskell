{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Convolutional.Activation where

import           AutoDiff
import           Network.CommonTypes

type Activation = Dual Number -> Dual Number
