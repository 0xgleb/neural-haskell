import Prelude hiding ((!!), (++), head, tail, foldl, foldr, zipWith, map, sum)
import Data.Type.Natural
import Data.Vector.Sized

import System.Random
import Control.Monad
import Control.Lens

import AutoDiff
import Network.Types
import Network

summ :: Num a => Vector (Dual a) n -> (Dual a) -> Vector a n -> Dual a
summ ws b a = (+ b) $ sum $ zipWithSame (*) ws $ map constDual a

logistic :: Floating a => a -> a
logistic x = 1 / (1 + exp (-x))

logisticNeuron :: Weights n -> Bias -> Neuron n
logisticNeuron = Neuron summ logistic

linearNeuron :: Weights n -> Bias -> Neuron n
linearNeuron = Neuron summ id

testNet :: [Number] -> [Bias] -> [Number] -> Number -> Network Two Two One
testNet [w11, w12, w21, w22, w31, w32, w41, w42] [b1, b2, b3, b4] [w1, w2, w3, w4] b = 
    (logisticNeuron (w11 :- w12 :- Nil) b1
  :- logisticNeuron (w21 :- w22 :- Nil) b2
  :- logisticNeuron (w31 :- w32 :- Nil) b3
  :- logisticNeuron (w41 :- w42 :- Nil) b4
  :- Nil) :~~ (logisticNeuron (w1 :- w2 :- w3 :- w4 :- Nil) b :- Nil) :~~ NilNetwork

examples = (Example (0 :- 0 :- Nil) (0 :- Nil))
        :- (Example (0 :- 1 :- Nil) (1 :- Nil))
        :- (Example (1 :- 0 :- Nil) (1 :- Nil))
        :- (Example (1 :- 1 :- Nil) (0 :- Nil))
        :- Nil

initNet :: IO (Network Two Two One)
initNet = (liftM testNet $ replicateM 8 (randomIO :: IO Number)) <*> replicateM 4 (randomIO :: IO Number) <*> replicateM 4 (randomIO :: IO Number) <*> (randomIO :: IO Number)

main :: IO ()
main = do
    smartNet <- teach squareError 1 examples (Right $ Right 60000) <$> initNet
    foldr ((>>) . print . round . head . runNetwork smartNet . (^. input)) mempty examples
