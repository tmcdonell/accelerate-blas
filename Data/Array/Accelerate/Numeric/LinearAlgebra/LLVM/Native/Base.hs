{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
-- |
-- Module      : Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.Native.Base
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.Native.Base
  where

import Data.Array.Accelerate.Data.Complex
import Data.Array.Accelerate.Array.Sugar                            ( Array(..), EltRepr )
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Unique
import Data.Array.Accelerate.Numeric.LinearAlgebra.Type

import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Complex                                     ( )

import qualified Blas.Primitive.Types                               as C


encodeTranspose :: Transpose -> C.Transpose
encodeTranspose N = C.NoTrans
encodeTranspose T = C.Trans
encodeTranspose H = C.ConjTrans


withArray
    :: forall sh e b. Numeric e
    => Array sh e
    -> (ArrayPtrs (EltRepr e) -> IO b)
    -> IO b
withArray (Array _ adata) = withArrayData (numericR::NumericR e) adata

withArrayData
    :: NumericR e
    -> ArrayData (EltRepr e)
    -> (ArrayPtrs (EltRepr e) -> IO b)
    -> IO b
withArrayData NumericRfloat32   (AD_Float  ua)    f = withUniqueArrayPtr ua f
withArrayData NumericRfloat64   (AD_Double ua)    f = withUniqueArrayPtr ua f
withArrayData NumericRcomplex32 (AD_Pair ad1 ad2) f
  | AD_Pair AD_Unit (AD_Float ua_re)  <- ad1
  , AD_Float ua_im                    <- ad2
  = withUniqueArrayPtr ua_re $ \p_re ->
    withUniqueArrayPtr ua_im $ \p_im ->
      f (((),p_re), p_im)

withArrayData NumericRcomplex64 (AD_Pair ad1 ad2) f
  | AD_Pair AD_Unit (AD_Double ua_re) <- ad1
  , AD_Double ua_im                   <- ad2
  = withUniqueArrayPtr ua_re $ \p_re ->
    withUniqueArrayPtr ua_im $ \p_im ->
      f (((),p_re), p_im)


interleave
    :: forall e b. (Storable e, Numeric (Complex e))
    => ArrayPtrs (EltRepr (Complex e))
    -> Int
    -> (Ptr (Complex e) -> IO b)
    -> IO b
interleave (((), p_re), p_im) n k = do
  allocaBytesAligned (n * sizeOf (undefined::Complex e)) 16 $ \p_cplx -> do
    () <- case numericR :: NumericR (Complex e) of
            NumericRcomplex32 -> c_interleave_f32 0 n p_cplx p_re p_im
            NumericRcomplex64 -> c_interleave_f64 0 n p_cplx p_re p_im
    --
    k p_cplx


deinterleave
    :: forall e. (Storable e, Numeric (Complex e))
    => ArrayPtrs (EltRepr (Complex e))
    -> Ptr (Complex e)
    -> Int
    -> IO ()
deinterleave (((), p_re), p_im) p_cplx n =
  case numericR :: NumericR (Complex e) of
    NumericRcomplex32 -> c_deinterleave_f32 0 n p_re p_im p_cplx
    NumericRcomplex64 -> c_deinterleave_f64 0 n p_re p_im p_cplx


foreign import ccall unsafe "interleave_f32"
  c_interleave_f32 :: Int -> Int -> Ptr (Complex Float) -> Ptr Float -> Ptr Float -> IO ()

foreign import ccall unsafe "interleave_f64"
  c_interleave_f64 :: Int -> Int -> Ptr (Complex Double) -> Ptr Double -> Ptr Double -> IO ()

foreign import ccall unsafe "deinterleave_f32"
  c_deinterleave_f32 :: Int -> Int -> Ptr Float -> Ptr Float -> Ptr (Complex Float) -> IO ()

foreign import ccall unsafe "deinterleave_f64"
  c_deinterleave_f64 :: Int -> Int -> Ptr Double -> Ptr Double -> Ptr (Complex Double) -> IO ()

