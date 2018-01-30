{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Module      : Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Base
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Base
  where

import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Sugar                            ( Array(..), EltRepr )
import Data.Array.Accelerate.Lifetime
import Data.Array.Accelerate.Numeric.LinearAlgebra.Type
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.PTX.Foreign

import Foreign.CUDA.Ptr                                             ( DevicePtr )
import qualified Foreign.CUDA.BLAS                                  as C


type family DevicePtrs e :: *
type instance DevicePtrs Float       = DevicePtr Float
type instance DevicePtrs Double      = DevicePtr Double
type instance DevicePtrs (V2 Float)  = DevicePtr Float
type instance DevicePtrs (V2 Double) = DevicePtr Double


encodeTranspose :: Transpose -> C.Operation
encodeTranspose N = C.N
encodeTranspose T = C.T
encodeTranspose H = C.C


withArray
    :: forall sh e b. Numeric e
    => Array sh e
    -> Stream
    -> (DevicePtrs (EltRepr e) -> LLVM PTX b)
    -> LLVM PTX b
withArray (Array _ adata) s k = withArrayData (numericR::NumericR e) adata s k

withArrayData
    :: NumericR e
    -> ArrayData (EltRepr e)
    -> Stream
    -> (DevicePtrs (EltRepr e) -> LLVM PTX b)
    -> LLVM PTX b
withArrayData NumericRfloat32 ad s k =
  withDevicePtr ad $ \p -> do
    r <- k p
    e <- checkpoint s
    return (Just e,r)
withArrayData NumericRfloat64 ad s k =
  withDevicePtr ad $ \p -> do
    r <- k p
    e <- checkpoint s
    return (Just e, r)
withArrayData NumericRcomplex32 (AD_V2 ad) s k =
  withDevicePtr ad $ \p -> do
    r <- k p
    e <- checkpoint s
    return (Just e,r)
withArrayData NumericRcomplex64 (AD_V2 ad) s k =
  withDevicePtr ad $ \p -> do
    r <- k p
    e <- checkpoint s
    return (Just e, r)


withLifetime' :: Lifetime a -> (a -> LLVM PTX b) -> LLVM PTX b
withLifetime' l k = do
  r <- k (unsafeGetValue l)
  liftIO $ touchLifetime l
  return r

