{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Module      : Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Level3
-- Copyright   : [2017..2020] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Level3
  where

import Data.Array.Accelerate.Data.Complex
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.Sugar.Elt

import Data.Array.Accelerate.LLVM.PTX.Foreign
import Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Base
import Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Context
import Data.Array.Accelerate.Numeric.LinearAlgebra.Type

import Foreign.Marshal                                              ( with )
import qualified Foreign.CUDA.Ptr                                   as CUDA
import qualified Foreign.CUDA.BLAS                                  as BLAS

import Control.Monad.Reader


-- NOTE: cuBLAS requires that matrices are stored in column-major order
-- (Fortran-style), but Accelerate uses a C-style convention where matrices are
-- stored in row-major order.
--
-- At least for matrix-matrix multiply, we can get around this problem by making
-- use of the equivalence \( B^T \cdot A^T = (A \cdot B)^T \).
--
gemm :: NumericR s e
     -> Transpose
     -> Transpose
     -> ForeignAcc (((((), Scalar e), Matrix e), Matrix e) -> Matrix e)
gemm nR opA opB = ForeignAcc "ptx.gemm" (gemm' nR opA opB)

gemm' :: NumericR s e
      -> Transpose
      -> Transpose
      -> ((((), Scalar e), Matrix e), Matrix e)
      -> Par PTX (Future (Matrix e))
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

      aR      = ArrayR dim2 eR
      eR      = case nR of
                  NumericRfloat32   -> eltR @Float
                  NumericRfloat64   -> eltR @Double
                  NumericRcomplex32 -> eltR @(Complex Float)
                  NumericRcomplex64 -> eltR @(Complex Double)

  --
  future <- new
  stream <- asks ptxStream
  matC   <- allocateRemote aR (((), m), n)
  alpha' <- indexRemote eR alpha 0
  ()     <- liftPar $
    withArray nR matA stream   $ \ptr_A -> do
     withArray nR matB stream  $ \ptr_B -> do
      withArray nR matC stream $ \ptr_C -> do
        withBLAS               $ \hdl   -> do
          case nR of
            NumericRfloat32 -> liftIO $
              with alpha' $ \ptr_alpha ->
               with 0     $ \ptr_beta  ->
                BLAS.sgemm hdl opB' opA' n m k ptr_alpha ptr_B ldb ptr_A lda ptr_beta ptr_C n

            NumericRfloat64 -> liftIO $
              with alpha' $ \ptr_alpha ->
               with 0     $ \ptr_beta  ->
                BLAS.dgemm hdl opB' opA' n m k ptr_alpha ptr_B ldb ptr_A lda ptr_beta ptr_C n

            NumericRcomplex32 -> liftIO $
              withV2 nR alpha' $ \ptr_alpha ->
               with 0          $ \ptr_beta  ->
                BLAS.cgemm hdl opB' opA' n m k ptr_alpha (CUDA.castDevPtr ptr_B) ldb (CUDA.castDevPtr ptr_A) lda ptr_beta (CUDA.castDevPtr ptr_C) n

            NumericRcomplex64 -> liftIO $
              withV2 nR alpha' $ \ptr_alpha ->
               with 0          $ \ptr_beta  ->
                BLAS.zgemm hdl opB' opA' n m k ptr_alpha (CUDA.castDevPtr ptr_B) ldb (CUDA.castDevPtr ptr_A) lda ptr_beta (CUDA.castDevPtr ptr_C) n
  --
  put future matC
  return future

