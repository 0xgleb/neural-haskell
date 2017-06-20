{-# LANGUAGE DataKinds #-}

import Prelude hiding ((!!), (++), head, tail, foldl, foldr, zipWith, map, sum)
import GHC.TypeLits

import System.Random
import Control.Monad
import Control.Lens hiding (cons)

import AutoDiff
import Network hiding (replicateM)
import Network.Neurons

testNet :: Network 2 2 1
testNet = (cons logisticNeuron $ cons logisticNeuron $ cons logisticNeuron $ cons logisticNeuron $ empty) :~~ singleton logisticNeuron :~~ NilNetwork

examples :: Vector 4 (Example 2 1)
examples = cons (Example (cons 0 $ cons 0 empty) (singleton 0))
         $ cons (Example (cons 0 $ cons 1 empty) (singleton 1))
         $ cons (Example (cons 1 $ cons 0 empty) (singleton 1))
         $ cons (Example (cons 1 $ cons 1 empty) (singleton 0))
         $ empty

main :: IO ()
main = do
    smartNet <- train squareError 1 examples (Right $ Right 60000) <$> initNet testNet
    foldr ((>>) . print . round . head . runNetwork smartNet . (^. input)) mempty examples
