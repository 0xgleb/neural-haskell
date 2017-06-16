{-# LANGUAGE StandaloneDeriving, DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}

module AutoDiffSpec (main, spec) where

import Prelude hiding (map, zipWith, head, last)

import Test.Hspec
import Test.QuickCheck

import AutoDiff
import Data.Vector.Sized

-- deriving instance Arbitrary (Dual a)

f :: Num a => a -> a
f x = 6*x^5 + 2*x^4 + 9*x^3 + 3*x^2 + x + 61
f' :: Double -> Double
f' x = 30*x^4 + 8*x^3 + 27*x^2 + 6*x + 1

logistic :: Floating a => a -> a
logistic x = 1 / (1 + exp (- x))
logistic' :: Double -> Double
logistic' x = logistic x * (1 - logistic x)

softplus :: Floating a => a -> a
softplus x = log (exp x + 1)

logisticZeroDerivative :: Num a => a
logisticZeroDerivative = 37

tanh' :: Double -> Double
tanh' x = (1 + tanh x) * (1 - tanh x)

tanhZeroDerivative :: Num a => a
tanhZeroDerivative = 20

spec :: Spec
spec = do
    describe "constDual" $ do
        it "returns a Dual number Dual (num) 0" $ do
            property $ \x -> val (constDual x) == (x :: Double)
    describe "d" $ do
        it "can differentiate polynomials" $ do
            property $ \x -> abs x < 1000 ==> floor (abs $ (d f x) - f' x) == 0
        it "can differentiate the logistic function" $ do
            property $ \x -> abs x < logisticZeroDerivative ==> floor (abs $ d logistic x - logistic' x) == 0
        it "can differentiate the softplus function" $ do
            property $ \x -> abs x < logisticZeroDerivative ==> floor (abs $ d softplus x - logistic x :: Double) == 0
        it "can differentiate the hyperbolic tangent" $ do
            property $ \x -> abs x < tanhZeroDerivative ==> floor (abs $ d tanh x - tanh' x) == 0
    describe "grad" $ do
        it "can calculate gradients of sum of polynomials of different arguments" $ do
            property $ \x y -> (abs x :: Double) < 1000 && (abs y :: Double) < 1000 ==> 
                map (floor . abs) (zipWith (-) (grad (\v -> f (head v) + f (last v)) $ cons x $ singleton y) (cons (f' x) $ singleton $ f' y)) == (cons 0 $ singleton 0 :: Vector 2 Int)
        it "can calculate gradients of polynomials" $ do
            property $ \x y -> (grad (\v -> head v * last v) $ cons (x :: Double) $ singleton (y :: Double)) == (y `cons` singleton x)

main :: IO ()
main = hspec spec
