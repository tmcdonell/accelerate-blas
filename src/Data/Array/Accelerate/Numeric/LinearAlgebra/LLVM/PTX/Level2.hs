{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Module      : Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Level2
-- Copyright   : [2017..2020] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Level2
  where

import Data.Complex
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.Sugar.Elt

import Data.Array.Accelerate.LLVM.PTX.Foreign
import Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Base
import Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Context
import Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Level3
import Data.Array.Accelerate.Numeric.LinearAlgebra.Type

import Foreign.Marshal                                              ( with )
import qualified Foreign.CUDA.Ptr                                   as CUDA
import qualified Foreign.CUDA.BLAS                                  as BLAS

import Control.Monad.Reader


-- NOTE: cuBLAS requires matrices to be stored in column-major order
-- (Fortran-style), but Accelerate uses C-style arrays in row-major order.
--
-- If the operation is N or T, we can just swap the operation. For
-- conjugate-transpose (H) operations (on complex valued arguments), since there
-- is no conjugate-no-transpose operation, we implement that via 'gemm', which
-- I assume is more efficient than ?geam followed by ?gemv.
--
gemv :: NumericR s e
     -> Transpose
     -> ForeignAcc ((((((), Scalar e), Matrix e), Vector e)) -> Vector e)
gemv eR opA = ForeignAcc "ptx.gemv" (gemv' eR opA)

gemv' :: NumericR s e
      -> Transpose
      -> ((((), Scalar e), Matrix e), Vector e)
      -> Par PTX (Future (Vector e))
gemv' NumericRcomplex32 H = as_gemm NumericRcomplex32 H
gemv' NumericRcomplex64 H = as_gemm NumericRcomplex64 H
gemv' nR                t = as_gemv nR t


as_gemm
    :: NumericR s e
    -> Transpose
    -> ((((), Scalar e), Matrix e), Vector e)
    -> Par PTX (Future (Vector e))
as_gemm nR opA ((((), alpha), matA), Array sh adata) = do
  let matB = Array (sh,1) adata
  --
  future <- new
  result <- gemm' nR opA N ((((), alpha), matA), matB)
  fork $ do Array (sh',_) vecy <- get result
            put future (Array sh' vecy)
  return future

as_gemv
    :: NumericR s e
    -> Transpose
    -> ((((), Scalar e), Matrix e), Vector e)
    -> Par PTX (Future (Vector e))
as_gemv nR opA ((((), alpha), matA), vecx) = do
  let
      (((), rowsA), colsA) = shape matA

      sizeY   = case opA of
                  N -> rowsA
                  _ -> colsA

      opA'    = encodeTranspose
              $ case opA of
                  N -> T
                  _ -> N

      aR      = ArrayR dim1 eR
      eR      = case nR of
                  NumericRfloat32   -> eltR @Float
                  NumericRfloat64   -> eltR @Double
                  NumericRcomplex32 -> eltR @(Complex Float)
                  NumericRcomplex64 -> eltR @(Complex Double)
  --
  future  <- new
  stream  <- asks ptxStream
  vecy    <- allocateRemote aR ((), sizeY)
  alpha'  <- indexRemote eR alpha 0
  ()      <- liftPar $ do
    withArray nR matA stream   $ \ptr_A -> do
     withArray nR vecx stream  $ \ptr_x -> do
      withArray nR vecy stream $ \ptr_y -> do
       withBLAS                $ \hdl   -> do
         case nR of
           NumericRfloat32 -> liftIO $
            with alpha' $ \ptr_alpha ->
             with 0     $ \ptr_beta  ->
               BLAS.sgemv hdl opA' colsA rowsA ptr_alpha ptr_A colsA ptr_x 1 ptr_beta ptr_y 1

           NumericRfloat64 -> liftIO $
            with alpha' $ \ptr_alpha ->
             with 0     $ \ptr_beta  ->
               BLAS.dgemv hdl opA' colsA rowsA ptr_alpha ptr_A colsA ptr_x 1 ptr_beta ptr_y 1

           NumericRcomplex32 -> liftIO $
            withV2 nR alpha' $ \ptr_alpha ->
             with 0          $ \ptr_beta  ->
               BLAS.cgemv hdl opA' colsA rowsA ptr_alpha (CUDA.castDevPtr ptr_A) colsA (CUDA.castDevPtr ptr_x) 1 ptr_beta (CUDA.castDevPtr ptr_y)  1

           NumericRcomplex64 -> liftIO $
            withV2 nR alpha' $ \ptr_alpha ->
             with 0          $ \ptr_beta  ->
               BLAS.zgemv hdl opA' colsA rowsA ptr_alpha (CUDA.castDevPtr ptr_A) colsA (CUDA.castDevPtr ptr_x) 1 ptr_beta (CUDA.castDevPtr ptr_y)  1
  --
  put future vecy
  return future

