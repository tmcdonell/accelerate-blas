{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Level2
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Level2
  where

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Array.Sugar                            ( Array(..) )
import Data.Array.Accelerate.LLVM.PTX.Foreign
import Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Base
import Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Context
import Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Level3
import Data.Array.Accelerate.Numeric.LinearAlgebra.Type

import Foreign.Marshal                                              ( with )
import qualified Foreign.CUDA.Ptr                                   as CUDA
import qualified Foreign.CUDA.BLAS                                  as BLAS


-- NOTE: cuBLAS requires matrices to be stored in column-major order
-- (Fortran-style), but Accelerate uses C-style arrays in row-major order.
--
-- If the operation is N or T, we can just swap the operation. For
-- conjugate-transpose (H) operations (on complex valued arguments), since there
-- is no conjugate-no-transpose operation, we implement that via 'gemm', which
-- I assume is more efficient than ?geam followed by ?gemv.
--
gemv :: Numeric e
     => Transpose
     -> ForeignAcc ((Scalar e, Matrix e, Vector e) -> Vector e)
gemv opA = ForeignAcc "ptx.gemv" (gemv' numericR opA)

gemv' :: Numeric e
      => NumericR e
      -> Transpose
      -> Stream
      -> (Scalar e, Matrix e, Vector e)
      -> LLVM PTX (Vector e)
gemv' NumericRcomplex32 H = as_gemm H
gemv' NumericRcomplex64 H = as_gemm H
gemv' _                 t = as_gemv t


as_gemm
    :: Numeric e
    => Transpose
    -> Stream
    -> (Scalar e, Matrix e, Vector e)
    -> LLVM PTX (Vector e)
as_gemm opA stream (alpha, matA, Array sh adata) = do
  let matB = Array (sh,1) adata
  --
  Array (sh',1) vecy <- gemm' opA N stream (alpha, matA, matB)
  return (Array sh' vecy)

as_gemv
    :: forall e. Numeric e
    => Transpose
    -> Stream
    -> (Scalar e, Matrix e, Vector e)
    -> LLVM PTX (Vector e)
as_gemv opA stream (alpha, matA, vecx) = do
  let
      Z :. rowsA :. colsA = arrayShape matA

      sizeY   = case opA of
                  N -> rowsA
                  _ -> colsA

      opA'    = encodeTranspose
              $ case opA of
                  N -> T
                  _ -> N
  --
  vecy    <- allocateRemote (Z :. sizeY) :: LLVM PTX (Vector e)
  alpha'  <- indexRemote alpha 0
  ()      <- do
    withArray matA stream   $ \ptr_A -> do
     withArray vecx stream  $ \ptr_x -> do
      withArray vecy stream $ \ptr_y -> do
       withBLAS             $ \hdl   -> do
         case numericR :: NumericR e of
           NumericRfloat32 -> liftIO $
            with alpha' $ \ptr_alpha ->
             with 0     $ \ptr_beta  ->
               BLAS.sgemv hdl opA' colsA rowsA ptr_alpha ptr_A colsA ptr_x 1 ptr_beta ptr_y 1

           NumericRfloat64 -> liftIO $
            with alpha' $ \ptr_alpha ->
             with 0     $ \ptr_beta  ->
               BLAS.dgemv hdl opA' colsA rowsA ptr_alpha ptr_A colsA ptr_x 1 ptr_beta ptr_y 1

           NumericRcomplex32 -> liftIO $
            with alpha' $ \ptr_alpha ->
             with 0     $ \ptr_beta  ->
               BLAS.cgemv hdl opA' colsA rowsA ptr_alpha (CUDA.castDevPtr ptr_A) colsA (CUDA.castDevPtr ptr_x) 1 ptr_beta (CUDA.castDevPtr ptr_y)  1

           NumericRcomplex64 -> liftIO $
            with alpha' $ \ptr_alpha ->
             with 0     $ \ptr_beta  ->
               BLAS.zgemv hdl opA' colsA rowsA ptr_alpha (CUDA.castDevPtr ptr_A) colsA (CUDA.castDevPtr ptr_x) 1 ptr_beta (CUDA.castDevPtr ptr_y)  1
  --
  return vecy

