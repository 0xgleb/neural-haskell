{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}

module Network.Convolutional.Network where

import Prelude hiding ((++), replicate, foldl, head, tail, length, sequence)
import Data.Vector.Sized
import qualified Data.Vector as Vec
import GHC.TypeLits
import Data.Proxy

import AutoDiff
import Network.CommonTypes
import Network.Convolutional.Types
import Network.Convolutional.Pool
import Network.Convolutional.Kernel
import Network.Convolutional.Activation

type KN n = KnownNat n

data Network iw ih id ow oh od where
    NilNet :: (NZ iw iw', NZ ih ih') => Network w h d w h d
    ActivL :: Activation -> Network iw ih id ow oh od -> Network iw ih id ow oh od
    FuConL :: (NZ d1 d1') => Vector d1 (Kernel iw ih d2) -> Network 1 1 d1 ow oh od -> Network iw ih d2 ow oh od
    Pad    :: (KN pw, KN ph, KN iw) => Proxy pw -> Proxy ph -> Network (pw + iw + pw) (ph + ih + ph) id ow oh od -> Network iw ih id ow oh od
    PoolL  :: (KN (w + 1), KN (h + 1), KN ph, KN pw, KN ws, KN hs, NZ ph ph') => Pool pw ph -> Proxy ws -> Proxy hs -> Network (w + 1) (h + 1) id ow oh od -> Network ((w * ws) + pw) ((h * hs) + ph) id ow oh od
    ConvL  :: (NZ d1 d1', KN d2, NZ d2 d2', KN kw, NZ kw kw', KN kh, NZ kh kh', KN (w + 1), KN (h + 1), KN ws, KN hs) => 
              Vector d1 (Kernel kw kh d2) -> Proxy ws -> Proxy hs -> Network (w + 1) (h + 1) d1 ow oh od -> Network ((w * ws) + kw) ((h * hs) + kh) d2 ow oh od

vectorToProxy :: Kernel w h d -> (Proxy h, Proxy w)
vectorToProxy _ = (Proxy, Proxy)

poolToProxy :: Pool w h -> (Proxy h, Proxy w)
poolToProxy _ = (Proxy, Proxy)

unsafeUnMaybe :: Maybe a -> a
unsafeUnMaybe (Just x) = x

unsafeTransposeV :: (KN m, NZ m m', KN n, NZ n n') => Vector n (Vector m a) -> Vector m (Vector n a)
unsafeTransposeV vector = unsafeUnMaybe $ toSized $ fmap (unsafeUnMaybe . toSized) $ unsizedTranspose $ fmap fromSized $ fromSized vector
    where unsizedTranspose :: Vec.Vector (Vec.Vector a) -> Vec.Vector (Vec.Vector a)
          unsizedTranspose v = if Vec.length v == 0 then Vec.empty else if Vec.length (Vec.head v) == 0 then Vec.empty else Vec.cons (fmap Vec.head v) $ unsizedTranspose (Vec.map Vec.tail v)


unsafeSplitVector :: (KN kl, KN s, KN (l + 1)) => Proxy kl -> Proxy s -> Vector ((l * s) + kl) a -> Vector (l + 1) (Vector kl a)
unsafeSplitVector kernelP strideP vector = unsafeUnMaybe $ toSized $ fmap (unsafeUnMaybe . toSized) $ splitUnsizedVector (fromInteger $ natVal kernelP) (fromInteger $ natVal strideP) $ fromSized vector
    where splitUnsizedVector :: Int -> Int -> Vec.Vector a -> Vec.Vector (Vec.Vector a)
          splitUnsizedVector kernelWidth stride vector = if Vec.length vector == kernelWidth then Vec.singleton vector else Vec.cons (Vec.take kernelWidth vector) $ splitUnsizedVector kernelWidth stride $ Vec.drop stride vector


runNetwork :: Network iw ih id ow oh od -> Image iw ih id -> Image ow oh od
runNetwork NilNet image = image
runNetwork (ActivL f netTail) image = runNetwork netTail $ fmap (fmap $ fmap $ toNormalF f) image
runNetwork (FuConL kernels netTail) image = runNetwork netTail $ fmap (\k -> singleton $ singleton $ val $ (summation k) (fmap (fmap $ fmap constDual) $ weights k) (constDual $ bias k) $ fmap (fmap $ fmap constDual) image) kernels
runNetwork (Pad wPad hPad netTail) image = runNetwork netTail $ fmap (\h -> zerosH ++ fmap (\v -> zerosW ++ v ++ zerosW) h ++ zerosH) image
    where zerosH = replicate' hPad $ zerosW ++ replicate 0 ++ zerosW
          zerosW = replicate' wPad 0
runNetwork (PoolL poolF strideW strideH netTail) image = runNetwork netTail $ fmap (fmap (fmap (val . poolF . fmap (fmap constDual)) . unsafeTransposeV . fmap (unsafeSplitVector (snd proxies) strideW)) . unsafeSplitVector (fst proxies) strideH) image
    where proxies = poolToProxy poolF
runNetwork (ConvL kernels strideW strideH netTail) image = runNetwork netTail $ convolve (\k i -> val $ (summation k) (fmap (fmap $ fmap constDual) $ weights k) (constDual $ bias k) $ fmap (fmap $ fmap constDual) i) kernels strideW strideH image
    where splitImage :: (KN (w + 1), KN (h + 1), KN ws, KN hs, KN d2, NZ d2 d2', KN kw, NZ kw kw', KN kh, NZ kh kh') => (Proxy kh, Proxy kw) -> Proxy ws -> Proxy hs -> Image ((w * ws) + kw) ((h * hs) + kh) d2 -> Vector (h + 1) (Vector (w + 1) (Image kw kh d2))
          splitImage proxies strideW strideH = fmap unsafeTransposeV . unsafeTransposeV . fmap (fmap unsafeTransposeV) . fmap (fmap $ fmap $ unsafeSplitVector (snd proxies) strideW) . fmap (unsafeSplitVector (fst proxies) strideH)
          convolve :: (KN (w + 1), KN (h + 1), KN ws, KN hs, NZ d1 d1', KN d2, NZ d2 d2', KN kw, NZ kw kw', KN kh, NZ kh kh') => 
                      (Kernel kw kh d2 -> Image kw kh d2 -> Number) -> Vector d1 (Kernel kw kh d2) -> Proxy ws -> Proxy hs -> Image ((w * ws) + kw) ((h * hs) + kh) d2 -> Image (w + 1) (h + 1) d1
          convolve f kernels strideW strideH image = fmap (\k -> fmap (fmap $ f k) $ splitImage (vectorToProxy $ head kernels) strideW strideH image) kernels
