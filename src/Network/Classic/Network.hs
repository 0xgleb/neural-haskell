{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Network.Classic.Network
( module Network.Classic.Neuron
, module Network.Classic.Types
, Network(..)
, nonDiffSum
, initNet
, runNetwork
, shouldStop
, toNetwork
, getTotalLoss
, train
, cvTrain
) where

import           Control.Lens
import           Control.Monad          (liftM2)
import           GHC.TypeLits
import           Prelude                hiding (foldl, head, length, mapM, sum,
                                         tail, zipWith, (!!), (++))
import           System.Random

import           AutoDiff
import           Network.Classic.Neuron
import           Network.Classic.Types
import           Network.CommonTypes


infixr 5 :~~

data Network inputs layers outputs where
    NilNetwork :: (NZ i i') => Network i 0 i

    (:~~) :: (NZ i i', NZ o o', NZ m m')
          => Vector i (Neuron m)
          -> Network i l o
          -> Network m (l + 1) o


nonDiffSum :: (DualWeights n -> Dual Bias -> Activations n -> Dual Output)
           -> Weights n
           -> Bias
           -> Activations n
           -> Output
nonDiffSum summ w b i = val $ summ (constDual <$> w) (constDual b) i

initNet :: Network i l o -> IO (Network i l o)
initNet NilNetwork          = return NilNetwork
initNet (layer :~~ netTail) =
    liftM2 (:~~) (mapM initNeuron layer) $ initNet netTail

runNetwork :: Network i l o -> Vector i Input -> Vector o Output
runNetwork NilNetwork          inputs = inputs
runNetwork (layer :~~ netTail) inputs =
    runNetwork netTail $ val . flip runNeuron inputs <$> layer

type Stop = Either StopCriteria (Either Loss Iterations)

shouldStop :: Stop -> Loss -> Iterations -> Bool
shouldStop (Right (Left m))  loss _    = m >= loss
shouldStop (Right (Right i)) _    iter = i <= iter
shouldStop (Left criteria)   loss iter =
    (criteria ^. maxLoss) >= loss || (criteria ^. maxIteration) <= iter

toNetwork :: (NZ i i', NZ o o') => Vector o (Neuron i) -> Network i 1 o
toNetwork layer = layer :~~ NilNetwork

getTotalLoss :: LossFunction o -> TotalLossFunction m o
getTotalLoss lossF = TotalLossFunction $ \v1 v2 -> val $ sum $
    zipWith (unLossF lossF) (fmap constDual <$> v1) (fmap constDual <$> v2)

dAct :: Vector o (Neuron i) -> Vector i Input -> Vector o Output
dAct layer inputs = diff . flip runNeuron inputs <$> layer


updateLayer :: Vector o (Neuron i)
            -> Vector i Input
            -> Vector o Loss
            -> LearningRate
            -> Vector o (Neuron i)
updateLayer layer inputs curErr rate = zipWith updNeuron layer curErr
  where updNeuron n e =
          n & weights %~ (zipWith (\n p -> p - rate * n) $ scaleVector e inputs)
            & bias %~ (\b -> b - rate * e)


updateNetwork :: (NZ m m', NZ i i')
              => Vector i (Neuron m)
              -> Network i l o
              -> LossFunction o
              -> LearningRate
              -> Example m o
              -> (Vector i Loss, Network m (l + 1) o)

updateNetwork layer NilNetwork lossF rate (Example inputs expected) =
    (curErr, toNetwork $ updateLayer layer inputs curErr rate)
    where curErr = zipWith (*) (dAct layer inputs)
                 $ grad (unLossF lossF $ constDual <$> expected)
                 $ runNetwork (toNetwork layer) inputs

updateNetwork layer (nextL :~~ netTail) lossF rate (Example inputs expected) =
    (curErr, updateLayer layer inputs curErr rate :~~ snd atNextLayer)
    where curErr = zipWith (*) (dAct layer inputs)
                 $ fst atNextLayer `multiply` fmap _weights nextL

          multiply :: (NZ n n')
                   => Vector n Loss
                   -> Vector n (Weights i)
                   -> Vector i Loss
          multiply losses ws = (\v -> foldl (zipWith (+)) (head v) (tail v))
                             $ zipWith scaleVector losses ws

          atNextLayer = updateNetwork nextL netTail lossF rate $
              Example { _input = runNetwork (toNetwork layer) inputs
                      , _output = expected
                      }

train :: (NZ l l')
      => LossFunction o
      -> LearningRate
      -> Vector m (Example i o)
      -> Stop
      -> Network i l o
      -> Network i l o
train lossF rate examples stopCriteria network = trainNetwork network 0
    where trainNetwork net i
            | shouldStop stopCriteria (loss net) i = net
            | otherwise =
                  trainNetwork (foldl (\(layer :~~ netTail) -> snd . updateNetwork layer netTail lossF rate) net examples) (i + 1)

          loss net = unTotLossF (getTotalLoss lossF) (runNetwork net . _input <$> examples) $ _output <$> examples

cvTrain :: (NZ l l')
        => LossFunction o
        -> LearningRate
        -> Vector m (Example i o)
        -> Vector n (Example i o)
        -> Stop
        -> Network i l o
        -> Network i l o
cvTrain lossF rate trainingExamples validationExamples stopCriteria network =
    trainNetwork network 0
    where trainNetwork net i
            | shouldStop stopCriteria (loss net) i = net
            | otherwise = trainNetwork (foldl (\(layer :~~ netTail) -> snd . updateNetwork layer netTail lossF rate) net trainingExamples) (i + 1)

          loss net = unTotLossF (getTotalLoss lossF) (runNetwork net . _input <$> validationExamples) $ _output <$> validationExamples
