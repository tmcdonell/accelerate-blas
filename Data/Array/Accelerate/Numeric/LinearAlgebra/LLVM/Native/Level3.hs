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

import qualified Blas.Primitive.Types                               as C
import qualified Blas.Primitive.Unsafe                              as C


gemm :: forall e. Numeric e
     => Transpose
     -> Transpose
     -> ForeignAcc ((Scalar e, Matrix e, Matrix e) -> Matrix e)
gemm opA opB = ForeignAcc "native.gemm" gemm'
  where
    gemm' (alpha, matA, matB) = do
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
          alpha'  = indexArray alpha Z
      --
      matC  <- allocateRemote (Z :. m :. n) :: LLVM Native (Matrix e)
      ()    <- liftIO $ do
        withArray matA   $ \ptr_A -> do
         withArray matB  $ \ptr_B -> do
          withArray matC $ \ptr_C -> do
            case numericR :: NumericR e of
              NumericRfloat32  -> C.sgemm C.RowMajor opA' opB' m n k alpha' ptr_A lda ptr_B ldb 0 ptr_C n
              NumericRfloat64  -> C.dgemm C.RowMajor opA' opB' m n k alpha' ptr_A lda ptr_B ldb 0 ptr_C n
      --
      return matC

