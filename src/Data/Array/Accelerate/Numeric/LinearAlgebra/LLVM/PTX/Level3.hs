{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Level3
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Level3
  where

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.LLVM.PTX.Foreign
import Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Base
import Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Context
import Data.Array.Accelerate.Numeric.LinearAlgebra.Type

import Foreign.Marshal                                              ( with )
import qualified Foreign.CUDA.Ptr                                   as CUDA
import qualified Foreign.CUDA.BLAS                                  as BLAS


-- NOTE: cuBLAS requires that matrices are stored in column-major order
-- (Fortran-style), but Accelerate uses a C-style convention where matrices are
-- stored in row-major order.
--
-- At least for matrix-matrix multiply, we can get around this problem by making
-- use of the equivalence \( B^T \cdot A^T = (A \cdot B)^T \).
--
gemm :: Numeric e
     => Transpose
     -> Transpose
     -> ForeignAcc ((Scalar e, Matrix e, Matrix e) -> Matrix e)
gemm opA opB = ForeignAcc "ptx.gemm" (gemm' opA opB)

gemm'
    :: forall e. Numeric e
    => Transpose
    -> Transpose
    -> Stream
    -> (Scalar e, Matrix e, Matrix e)
    -> LLVM PTX (Matrix e)
gemm' opA opB stream (alpha, matA, matB) = do
  let
      Z :. rowsA :. colsA = arrayShape matA
      Z :. rowsB :. colsB = arrayShape matB

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
  --
  matC   <- allocateRemote (Z :. m :. n) :: LLVM PTX (Matrix e)
  alpha' <- indexRemote alpha 0
  ()     <- withArray matA stream   $ \ptr_A -> do
             withArray matB stream  $ \ptr_B -> do
              withArray matC stream $ \ptr_C -> do
                withBLAS            $ \hdl   -> do
                  case numericR :: NumericR e of
                    NumericRfloat32 -> liftIO $
                      with alpha' $ \ptr_alpha ->
                       with 0     $ \ptr_beta  ->
                        BLAS.sgemm hdl opB' opA' n m k ptr_alpha ptr_B ldb ptr_A lda ptr_beta ptr_C n

                    NumericRfloat64 -> liftIO $
                      with alpha' $ \ptr_alpha ->
                       with 0     $ \ptr_beta  ->
                        BLAS.dgemm hdl opB' opA' n m k ptr_alpha ptr_B ldb ptr_A lda ptr_beta ptr_C n

                    NumericRcomplex32 -> liftIO $
                      with alpha' $ \ptr_alpha ->
                       with 0     $ \ptr_beta  ->
                        BLAS.cgemm hdl opB' opA' n m k ptr_alpha (CUDA.castDevPtr ptr_B) ldb (CUDA.castDevPtr ptr_A) lda ptr_beta (CUDA.castDevPtr ptr_C) n

                    NumericRcomplex64 -> liftIO $
                      with alpha' $ \ptr_alpha ->
                       with 0     $ \ptr_beta  ->
                        BLAS.zgemm hdl opB' opA' n m k ptr_alpha (CUDA.castDevPtr ptr_B) ldb (CUDA.castDevPtr ptr_A) lda ptr_beta (CUDA.castDevPtr ptr_C) n

  return matC

