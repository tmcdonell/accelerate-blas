{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.Native.Level3
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.Native.Level3
  where

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.LLVM.Native.Foreign
import Data.Array.Accelerate.Numeric.LinearAlgebra.Type
import Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.Native.Base

import qualified Numerical.HBLAS.BLAS.Level3                        as H


gemm :: forall e. Numeric e
     => Transpose
     -> Transpose
     -> ForeignAcc ((Scalar e, Matrix e, Matrix e) -> Matrix e)
gemm opA opB = ForeignAcc "cblas.gemm" gemm'
  where
    gemm' (alpha, matA, matB) = do
      let
          Z :. rowsA :. colsA = arrayShape matA
          Z :. rowsB :. colsB = arrayShape matB

          rowsC   = case opA of
                      N -> rowsA
                      _ -> colsA
          colsC   = case opB of
                      N -> colsB
                      _ -> rowsB

          opA'    = encodeTranspose opA
          opB'    = encodeTranspose opB
          alpha'  = indexArray alpha Z
      --
      matC  <- allocateRemote (Z :. rowsC :. colsC) :: LLVM Native (Matrix e)
      liftIO $ do
       withMatrix matA   $ \matA' -> do
        withMatrix matB  $ \matB' -> do
         withMatrix matC $ \matC' -> do
          case numericR :: NumericR e of
            NumericRfloat32   -> H.sgemm opA' opB' alpha' 0 matA' matB' matC' >> return matC
            NumericRfloat64   -> H.dgemm opA' opB' alpha' 0 matA' matB' matC' >> return matC
            -- NumericRcomplex32 -> H.cgemm opA' opB' alpha' 0 matA' matB' matC' >> return matC
            -- NumericRcomplex64 -> H.zgemm opA' opB' alpha' 0 matA' matB' matC' >> return matC

