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

testNet :: [Number] -> [Bias] -> [Number] -> Number -> Network 2 2 1
testNet [w11, w12, w21, w22, w31, w32, w41, w42] [b1, b2, b3, b4] [w1, w2, w3, w4] b = 
    (cons (logisticNeuron (cons w11 $ cons w12 empty) b1)
   $ cons (logisticNeuron (cons w21 $ cons w22 empty) b2)
   $ cons (logisticNeuron (cons w31 $ cons w32 empty) b3)
   $ cons (logisticNeuron (cons w41 $ cons w42 empty) b4)
   $ empty) :~~ (cons (logisticNeuron (cons w1 $ cons w2 $ cons w3 $ cons w4 empty) b) empty) :~~ NilNetwork

examples = cons (Example (cons 0 $ cons 0 empty) (singleton 0))
         $ cons (Example (cons 0 $ cons 1 empty) (singleton 1))
         $ cons (Example (cons 1 $ cons 0 empty) (singleton 1))
         $ cons (Example (cons 1 $ cons 1 empty) (singleton 0))
         $ empty

initNet :: IO (Network 2 2 1)
initNet = (liftM testNet $ replicateM 8 (randomIO :: IO Number)) <*> replicateM 4 (randomIO :: IO Number) <*> replicateM 4 (randomIO :: IO Number) <*> (randomIO :: IO Number)

main :: IO ()
main = do
    smartNet <- teach squareError 1 examples (Right $ Right 60000) <$> initNet
    foldr ((>>) . print . round . head . runNetwork smartNet . (^. input)) mempty examples
