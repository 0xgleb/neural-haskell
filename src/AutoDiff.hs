{-# LANGUAGE TypeFamilies, DataKinds #-}

module AutoDiff 
( Dual(..)
, constDual
, d
, grad
) where

import Data.Vector.Sized
import Data.Type.Natural

data Dual a = Dual { val  :: a
                   , diff :: a
                   } deriving (Eq, Read, Show)

constDual :: Num a => a -> Dual a
constDual x = Dual x 0

-- to use we put Dual (f x) (f' x). Eg if using "x", then write Dual x 1, cause dx/dx = 1
instance Num a => Num (Dual a) where
    (Dual u u') + (Dual v v') = Dual (u + v) (u' + v')
    (Dual u u') - (Dual v v') = Dual (u - v) (u' - v')
    (Dual u u') * (Dual v v') = Dual (u * v) (u' * v + u * v')
    abs (Dual u u') = Dual (abs u) (u' * (signum u))
    signum (Dual u u') = Dual (signum u) 0
    fromInteger n = Dual (fromInteger n) 0

instance Fractional a => Fractional (Dual a) where
    (Dual u u') / (Dual v v') = Dual (u / v) ((u' * v - u * v') / v^2)
    fromRational q = Dual (fromRational q) 0

instance (Eq a, Floating a) => Floating (Dual a) where
    pi                = Dual pi        0
    exp   (Dual u u') = Dual (exp u)   (u' * (exp u))
    log   (Dual u u') = Dual (log u)   (u' / u)
    sqrt  (Dual u u') = Dual (sqrt u)  (u' / (2 * sqrt u))
    sin   (Dual u u') = Dual (sin u)   (u' * (cos u))
    cos   (Dual u u') = Dual (cos u)   (-1 * u' * (sin u))
    tan   (Dual u u') = Dual (tan u)   (1 / ((cos u) ** 2))
    asin  (Dual u u') = Dual (asin u)  (u' / (sqrt(1 - (u ** 2))))
    acos  (Dual u u') = Dual (acos u)  ((- 1) * u' / (sqrt(1 - (u ** 2))))
    atan  (Dual u u') = Dual (atan u)  (u' / (1 + (u ** 2)))
    sinh  (Dual u u') = Dual (sinh u)  (u' * cosh u)
    cosh  (Dual u u') = Dual (cosh u)  (u' * sinh u)
    tanh  (Dual u u') = Dual (tanh u)  (u' * (1 - ((tanh u) ** 2)))
    asinh (Dual u u') = Dual (asinh u) (u' / (sqrt(1 + (u ** 2))))
    acosh (Dual u u') = Dual (acosh u) ((u' / (sqrt((u ** 2) - 1))))
    atanh (Dual u u') = Dual (atanh u) (u' / (1 - (u ** 2)))
    (Dual u u') ** (Dual n 0)       = Dual (u ** n) (u' * n * u ** (n - 1))
    (Dual a 0)  ** (Dual v v')      = Dual (a ** v) (v' * log a * a ** v)
    (Dual u u') ** (Dual v v')      = Dual (u ** v) ((u ** v) * (v' * (log u) + (v * u' / u)))
    logBase (Dual u u') (Dual v v') = Dual (logBase u v) (((log v) * u' / u - (log u) * v' / v) / ((log u) ** 2))

instance Ord a => Ord (Dual a) where
    (Dual x _) <= (Dual y _ ) = x <= y

d :: (Num a, Num c) => (Dual a -> Dual c) -> a -> c
d f = \x -> diff . f $ Dual x 1

imap :: (Int -> a -> b) -> Vector a n -> Vector b n
imap f v = indexedMap 0 f v
    where indexedMap :: Int -> (Int -> a -> b) -> Vector a n -> Vector b n
          indexedMap _ _ Nil       = Nil :: Vector b Z
          indexedMap n f (x :- xs) = f n x :- indexedMap (n+1) f xs

grad :: Num a => (Vector (Dual a) n -> (Dual a)) -> Vector a n -> Vector a n
grad f = \v -> imap (\i _ -> diff $ f $ toDualVector v i) v
    where toDualVector v i = imap (\index value -> if index == i then Dual value 1 else Dual value 0) v
