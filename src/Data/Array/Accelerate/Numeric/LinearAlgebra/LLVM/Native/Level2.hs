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
import Data.Array.Accelerate.LLVM.Native.Foreign
import Data.Array.Accelerate.Numeric.LinearAlgebra.Type
import Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.Native.Base

import Foreign.Ptr
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

          sizeY   = case opA of
                      N -> rowsA
                      _ -> colsA

          opA'    = encodeTranspose opA
          alpha'  = indexArray alpha Z
      --
      future  <- new
      vecy    <- allocateRemote (Z :. sizeY)
      ()      <- liftIO $ do
        withArray matA   $ \ptr_A -> do
         withArray vecx  $ \ptr_x -> do
          withArray vecy $ \ptr_y -> do
            case numericR :: NumericR e of
              NumericRfloat32   -> C.sgemv C.RowMajor opA' rowsA colsA alpha' ptr_A colsA ptr_x 1 0 ptr_y 1
              NumericRfloat64   -> C.dgemv C.RowMajor opA' rowsA colsA alpha' ptr_A colsA ptr_x 1 0 ptr_y 1
              NumericRcomplex32 -> C.cgemv C.RowMajor opA' rowsA colsA alpha' (castPtr ptr_A) colsA (castPtr ptr_x) 1 0 (castPtr ptr_y) 1
              NumericRcomplex64 -> C.zgemv C.RowMajor opA' rowsA colsA alpha' (castPtr ptr_A) colsA (castPtr ptr_x) 1 0 (castPtr ptr_y) 1
      --
      put future vecy
      return future

