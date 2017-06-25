{-# LANGUAGE DataKinds #-}

import Prelude hiding ((!!), (++), head, tail, foldl, foldr, zipWith, map, sum)
import GHC.TypeLits

import System.Random
import Control.Monad
import Control.Lens hiding (cons)

import AutoDiff
import Network.CommonTypes
import Network.Classic hiding (replicateM)
import Network.Classic.Neurons

testNet :: Network 2 2 1
testNet = (cons sigmoidNeuron $ cons sigmoidNeuron $ cons sigmoidNeuron $ cons sigmoidNeuron $ empty) :~~ singleton sigmoidNeuron :~~ NilNetwork

examples :: Vector 4 (Example 2 1)
examples = cons (Example (cons 0 $ singleton 0) $ singleton 0)
         $ cons (Example (cons 0 $ singleton 1) $ singleton 1)
         $ cons (Example (cons 1 $ singleton 0) $ singleton 1)
         $ cons (Example (cons 1 $ singleton 1) $ singleton 0)
         $ empty

main :: IO ()
main = do
    smartNet <- train quadraticLoss 1 examples (Left $ StopCriteria 0.0000073 60000) <$> initNet testNet
    print $ (unTotLossF $ getTotalLoss quadraticLoss) (map _output examples) $ map (runNetwork smartNet . _input) examples
    putStrLn ""
    foldr ((>>) . print . head . runNetwork smartNet . (^. input)) mempty examples
