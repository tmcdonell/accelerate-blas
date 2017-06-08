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
import Data.Array.Accelerate.Data.Complex
import Data.Array.Accelerate.LLVM.PTX.Foreign
import Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Base
import Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Context
import Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Twine
import Data.Array.Accelerate.Numeric.LinearAlgebra.Type

import Foreign.Marshal                                              ( with )
import Foreign.Storable.Complex                                     ( )

import qualified Foreign.CUDA.Ptr                                   as CUDA
import qualified Foreign.CUDA.BLAS                                  as BLAS


gemm :: forall e. Numeric e
     => Transpose
     -> Transpose
     -> ForeignAcc ((Scalar e, Matrix e, Matrix e) -> Matrix e)
gemm opA opB = ForeignAcc "ptx.gemm" gemm'
  where
    gemm' stream (alpha, matA, matB) = do
      let
          Z :. rowsA :. colsA = arrayShape matA
          Z :. rowsB :. colsB = arrayShape matB

          sizeA   = rowsA * colsA
          sizeB   = rowsB * colsB
          sizeC   = m * n

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

                        NumericRcomplex32 -> do
                          tmpC <- allocateRemote (Z :. sizeC * 2) :: LLVM PTX (Vector Float)
                          withArray tmpC stream             $ \ptr_C' -> do
                            interleave ptr_A stream sizeA   $ \ptr_A' -> do
                              interleave ptr_B stream sizeB $ \ptr_B' -> do
                                liftIO $
                                  with alpha' $ \ptr_alpha ->
                                   with 0     $ \ptr_beta  ->
                                    BLAS.cgemm hdl opB' opA' n m k ptr_alpha ptr_B' ldb ptr_A' lda ptr_beta (CUDA.castDevPtr ptr_C') n
                                deinterleave ptr_C (CUDA.castDevPtr ptr_C' :: CUDA.DevicePtr (Complex Float)) stream sizeC

                        NumericRcomplex64 -> do
                          tmpC <- allocateRemote (Z :. sizeC * 2) :: LLVM PTX (Vector Double)
                          withArray tmpC stream             $ \ptr_C' -> do
                            interleave ptr_A stream sizeA   $ \ptr_A' -> do
                              interleave ptr_B stream sizeB $ \ptr_B' -> do
                                liftIO $
                                  with alpha' $ \ptr_alpha ->
                                   with 0     $ \ptr_beta  ->
                                    BLAS.zgemm hdl opB' opA' n m k ptr_alpha ptr_B' ldb ptr_A' lda ptr_beta (CUDA.castDevPtr ptr_C') n
                                deinterleave ptr_C (CUDA.castDevPtr ptr_C' :: CUDA.DevicePtr (Complex Double)) stream sizeC

      return matC

