{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Module      : Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.Native.Level2
-- Copyright   : [2017..2020] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.Native.Level2
  where

import Data.Complex
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.Sugar.Elt

import Data.Array.Accelerate.LLVM.Native.Foreign
import Data.Array.Accelerate.Numeric.LinearAlgebra.Type
import Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.Native.Base

import Foreign.Ptr
import qualified Blas.Primitive.Types                               as C
import qualified Blas.Primitive.Unsafe                              as C


gemv :: NumericR s e
     -> Transpose
     -> ForeignAcc ((((((), Scalar e), Matrix e), Vector e)) -> Vector e)
gemv nR opA = ForeignAcc "native.gemv" (gemv' nR opA)

gemv' :: NumericR s e
      -> Transpose
      -> ((((), Scalar e), Matrix e), Vector e)
      -> Par Native (Future (Vector e))
gemv' nR opA ((((), alpha), matA), vecx) = do
  let
      (((), rowsA), colsA) = shape matA

      sizeY   = case opA of
                  N -> rowsA
                  _ -> colsA

      opA'    = encodeTranspose opA
      alpha'  = indexArray (ArrayR dim0 eR) alpha ()

      aR      = ArrayR dim1 eR
      eR      = case nR of
                  NumericRfloat32   -> eltR @Float
                  NumericRfloat64   -> eltR @Double
                  NumericRcomplex32 -> eltR @(Complex Float)
                  NumericRcomplex64 -> eltR @(Complex Double)
  --
  future <- new
  vecy   <- allocateRemote aR ((), sizeY)
  ()     <- liftIO $ do
    withArray nR matA   $ \ptr_A -> do
     withArray nR vecx  $ \ptr_x -> do
      withArray nR vecy $ \ptr_y -> do
        case nR of
          NumericRfloat32   -> C.sgemv C.RowMajor opA' rowsA colsA alpha' ptr_A colsA ptr_x 1 0 ptr_y 1
          NumericRfloat64   -> C.dgemv C.RowMajor opA' rowsA colsA alpha' ptr_A colsA ptr_x 1 0 ptr_y 1
          NumericRcomplex32 -> C.cgemv C.RowMajor opA' rowsA colsA (toElt alpha') (castPtr ptr_A) colsA (castPtr ptr_x) 1 0 (castPtr ptr_y) 1
          NumericRcomplex64 -> C.zgemv C.RowMajor opA' rowsA colsA (toElt alpha') (castPtr ptr_A) colsA (castPtr ptr_x) 1 0 (castPtr ptr_y) 1
  --
  put future vecy
  return future

