{-# LANGUAGE DataKinds #-}

import Prelude hiding ((!!), (++), head, tail, foldl, foldr, zipWith, map, sum)
import Data.Vector.Sized hiding (replicateM)
import GHC.TypeLits

import System.Random
import Control.Monad
import Control.Lens hiding (cons)

import AutoDiff
import Network.Types
import Network

summ :: Num a => Vector n (Dual a) -> (Dual a) -> Vector n a -> Dual a
summ ws b a = (+ b) $ sum $ zipWith (*) ws $ map constDual a

logistic :: Floating a => a -> a
logistic x = 1 / (1 + exp (-x))

logisticNeuron :: Weights n -> Bias -> Neuron n
logisticNeuron = Neuron summ logistic

linearNeuron :: Weights n -> Bias -> Neuron n
linearNeuron = Neuron summ id

testNet :: Network 2 2 1
testNet = (cons (logisticNeuron (cons 0 $ singleton 0) 0)
         $ cons (logisticNeuron (cons 0 $ singleton 0) 0)
         $ cons (logisticNeuron (cons 0 $ singleton 0) 0)
         $ cons (logisticNeuron (cons 0 $ singleton 0) 0)
         $ empty) :~~ (cons (logisticNeuron (cons 0 $ cons 0 $ cons 0 $ cons 0 empty) 0) empty) :~~ NilNetwork

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
