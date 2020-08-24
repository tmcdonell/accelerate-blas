{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Module      : Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.Native.Level3
-- Copyright   : [2017..2020] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.Native.Level3
  where

import Data.Array.Accelerate.Data.Complex
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.Sugar.Elt

import Data.Array.Accelerate.LLVM.Native.Foreign
import Data.Array.Accelerate.Numeric.LinearAlgebra.Type
import Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.Native.Base

import Foreign.Ptr
import qualified Blas.Primitive.Types                               as C
import qualified Blas.Primitive.Unsafe                              as C


-- TODO: check whether it is faster to compute this as column-major order:
--
-- https://www.christophlassner.de/using-blas-from-c-with-row-major-data.html
--
gemm :: NumericR s e
     -> Transpose
     -> Transpose
     -> ForeignAcc (((((), Scalar e), Matrix e), Matrix e) -> Matrix e)
gemm nR opA opB = ForeignAcc "native.gemm" (gemm' nR opA opB)
  where

gemm' :: NumericR s e
      -> Transpose
      -> Transpose
      -> ((((), Scalar e), Matrix e), Matrix e)
      -> Par Native (Future (Matrix e))
gemm' nR opA opB ((((), alpha), matA), matB) = do
  let
      (((), rowsA), colsA) = shape matA
      (((), rowsB), colsB) = shape matB

      (m,k)   = case opA of
                  N -> (rowsA, colsA)
                  _ -> (colsA, rowsA)
      n       = case opB of
                  N -> colsB
                  _ -> rowsB

      lda     = colsA
      ldb     = colsB

      opA'    = encodeTranspose opA
      opB'    = encodeTranspose opB
      alpha'  = indexArray (ArrayR dim0 eR) alpha ()

      aR      = ArrayR dim2 eR
      eR      = case nR of
                  NumericRfloat32   -> eltR @Float
                  NumericRfloat64   -> eltR @Double
                  NumericRcomplex32 -> eltR @(Complex Float)
                  NumericRcomplex64 -> eltR @(Complex Double)

  --
  future <- new
  matC   <- allocateRemote aR (((), m), n)
  ()     <- liftIO $ do
    withArray nR matA   $ \ptr_A -> do
     withArray nR matB  $ \ptr_B -> do
      withArray nR matC $ \ptr_C -> do
        case nR of
          NumericRfloat32   -> C.sgemm C.RowMajor opA' opB' m n k alpha' ptr_A lda ptr_B ldb 0 ptr_C n
          NumericRfloat64   -> C.dgemm C.RowMajor opA' opB' m n k alpha' ptr_A lda ptr_B ldb 0 ptr_C n
          NumericRcomplex32 -> C.cgemm C.RowMajor opA' opB' m n k (toElt alpha') (castPtr ptr_A) lda (castPtr ptr_B) ldb 0 (castPtr ptr_C) n
          NumericRcomplex64 -> C.zgemm C.RowMajor opA' opB' m n k (toElt alpha') (castPtr ptr_A) lda (castPtr ptr_B) ldb 0 (castPtr ptr_C) n
  --
  put future matC
  return future

