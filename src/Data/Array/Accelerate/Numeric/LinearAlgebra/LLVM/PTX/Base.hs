{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UnboxedTuples       #-}
-- |
-- Module      : Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Base
-- Copyright   : [2017..2020] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Base
  where

import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Data.Complex
import Data.Array.Accelerate.Lifetime
import Data.Array.Accelerate.Numeric.LinearAlgebra.Type
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Type
import Data.Primitive.Vec

import Data.Array.Accelerate.LLVM.PTX.Foreign

import Foreign.CUDA.Ptr                                             ( DevicePtr )
import qualified Foreign.CUDA.BLAS                                  as C

import GHC.Base
import GHC.Ptr


type family DevicePtrs e :: *
type instance DevicePtrs Float         = DevicePtr Float
type instance DevicePtrs Double        = DevicePtr Double
type instance DevicePtrs (Vec2 Float)  = DevicePtr Float
type instance DevicePtrs (Vec2 Double) = DevicePtr Double


encodeTranspose :: Transpose -> C.Operation
encodeTranspose N = C.N
encodeTranspose T = C.T
encodeTranspose H = C.C


{-# INLINE withV2 #-}
withV2
    :: NumericR s (Vec2 a)
    -> Vec2 a
    -> (Ptr (Complex a) -> IO b)
    -> IO b
withV2 nR (Vec ba#) k =
  let !(I# bytes#) = case nR of
                       NumericRcomplex32 -> 8
                       NumericRcomplex64 -> 16
  in
  case isByteArrayPinned# ba# of
   1# -> IO $ \s0 ->
     case k (Ptr (byteArrayContents# ba#)) of { IO k#       ->
     case k# s0                            of { (# s1, r #) ->
     case touch# ba# s1                    of { s2          ->
       (# s2, r #) }}}
  --
   _  -> IO $ \s0 ->
     case newAlignedPinnedByteArray# bytes# 16# s0                        of { (# s1, mba# #) ->
     case copyAddrToByteArray# (byteArrayContents# ba#) mba# 0# bytes# s1 of { s2             ->
     case unsafeFreezeByteArray# mba# s2                                  of { (# s3, ba'# #) ->
     case k (Ptr (byteArrayContents# ba'#))                               of { IO k#          ->
     case k# s3                                                           of { (# s4, r #)    ->
     case touch# ba'# s4                                                  of { s5             ->
       (# s5, r #) }}}}}}

{-# INLINE withArray #-}
withArray
    :: NumericR s e
    -> Array sh e
    -> Stream
    -> (DevicePtrs e -> LLVM PTX b)
    -> LLVM PTX b
withArray eR (Array _ adata) s k =
  withArrayData eR adata s k

{-# INLINE withArrayData #-}
withArrayData
    :: NumericR s e
    -> ArrayData e
    -> Stream
    -> (DevicePtrs e -> LLVM PTX b)
    -> LLVM PTX b
withArrayData NumericRfloat32 ad s k =
  withDevicePtr (singleType @Float) ad $ \p -> do
    r <- k p
    e <- waypoint s
    return (Just e,r)
withArrayData NumericRfloat64 ad s k =
  withDevicePtr (singleType @Double) ad $ \p -> do
    r <- k p
    e <- waypoint s
    return (Just e, r)
withArrayData NumericRcomplex32 ad s k =
  withDevicePtr (singleType @Float) ad $ \p -> do
    r <- k p
    e <- waypoint s
    return (Just e,r)
withArrayData NumericRcomplex64 ad s k =
  withDevicePtr (singleType @Double) ad $ \p -> do
    r <- k p
    e <- waypoint s
    return (Just e, r)

{-# INLINE withLifetime' #-}
withLifetime' :: Lifetime a -> (a -> LLVM PTX b) -> LLVM PTX b
withLifetime' l k = do
  r <- k (unsafeGetValue l)
  liftIO $ touchLifetime l
  return r

