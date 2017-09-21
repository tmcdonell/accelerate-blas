{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.Native.Level2
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.Native.Level2
  where

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Data.Complex
import Data.Array.Accelerate.LLVM.Native.Foreign
import Data.Array.Accelerate.Numeric.LinearAlgebra.Type
import Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.Native.Base

import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Storable.Complex                                     ( )

import qualified Blas.Primitive.Types                               as C
import qualified Blas.Primitive.Unsafe                              as C


gemv :: forall e. Numeric e
     => Transpose
     -> ForeignAcc ((Scalar e, Matrix e, Vector e) -> Vector e)
gemv opA = ForeignAcc "native.gemv" gemv'
  where
    gemv' (alpha, matA, vecx) = do
      let
          Z :. rowsA :. colsA = arrayShape matA
          Z :. sizeX          = arrayShape vecx

          sizeA   = rowsA * colsA
          sizeY   = case opA of
                      N -> rowsA
                      _ -> colsA

          opA'    = encodeTranspose opA
          alpha'  = indexArray alpha Z
      --
      vecy  <- allocateRemote (Z :. sizeY) :: LLVM Native (Vector e)
      ()    <- liftIO $ do
        withArray matA   $ \ptr_A -> do
         withArray vecx  $ \ptr_x -> do
          withArray vecy $ \ptr_y -> do
            case numericR :: NumericR e of
              NumericRfloat32   -> C.sgemv C.RowMajor opA' rowsA colsA alpha' ptr_A colsA ptr_x 1 0 ptr_y 1
              NumericRfloat64   -> C.dgemv C.RowMajor opA' rowsA colsA alpha' ptr_A colsA ptr_x 1 0 ptr_y 1
              --
              NumericRcomplex32 -> do
                allocaBytesAligned (sizeY * sizeOf (undefined::Complex e)) 16 $ \ptr_y' -> do
                 interleave ptr_A sizeA  $ \ptr_A' -> do
                  interleave ptr_x sizeX $ \ptr_x' -> do
                    C.cgemv C.RowMajor opA' rowsA colsA alpha' ptr_A' colsA ptr_x' 1 0 ptr_y' 1
                    deinterleave ptr_y ptr_y' sizeY
              --
              NumericRcomplex64 -> do
                allocaBytesAligned (sizeY * sizeOf (undefined::Complex e)) 16 $ \ptr_y' -> do
                 interleave ptr_A sizeA  $ \ptr_A' -> do
                  interleave ptr_x sizeX $ \ptr_x' -> do
                    C.zgemv C.RowMajor opA' rowsA colsA alpha' ptr_A' colsA ptr_x' 1 0 ptr_y' 1
                    deinterleave ptr_y ptr_y' sizeY
        --
      return vecy

