{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications    #-}
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
import Data.Array.Accelerate.Type                                   as A
import qualified Data.Array.Accelerate.Representation.Array         as Repr
import qualified Data.Array.Accelerate.Representation.Shape         as Repr
import qualified Data.Array.Accelerate.Representation.Type          as Repr
import qualified Data.Array.Accelerate.Sugar.Array                  as Sugar
import qualified Data.Array.Accelerate.Sugar.Elt                    as Sugar

import Data.Array.Accelerate.LLVM.Native.Foreign
import Data.Array.Accelerate.Numeric.LinearAlgebra.Type
import Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.Native.Base

import Foreign.Ptr
import qualified Blas.Primitive.Types                               as C
import qualified Blas.Primitive.Unsafe                              as C

-- TODO: Numeric (Sugar.EltR e)?
gemv :: forall e . Numeric e
     => Transpose
     -> ForeignAcc (Sugar.ArraysR (Scalar e, Matrix e, Vector e) -> Sugar.ArraysR (Vector e))
gemv opA = ForeignAcc "native.gemv" go
  where
    go ((((), alpha :: Repr.Array Repr.DIM0 (Sugar.EltR e))
         , matA :: Repr.Array Repr.DIM2 (Sugar.EltR e))
         , vecx :: Repr.Array Repr.DIM1 (Sugar.EltR e)) = do
      let
        (((), rowsA), colsA) = Repr.shape matA
        sizeY = case opA of { N -> rowsA; _ -> colsA }
        opA' = encodeTranspose N
        t = case (numericR :: NumericR e) of
              NumericRfloat32 -> Repr.TupRsingle scalarType
              NumericRfloat64 -> Repr.TupRsingle scalarType
        -- TODO: is this the best way to write this? Seems like there should be
        -- a helper to construct the ArrayR?
        alpha' = Repr.indexArray (Repr.ArrayR Repr.dim0 t) alpha () :: Sugar.EltR e
      future <- new
      (vecy :: Repr.Array ((), Int) (Sugar.EltR e)) <- allocateRemote (Repr.ArrayR Repr.dim1 t) ((), sizeY)

      () <- liftIO $ do
        withArray @_ @e matA     $ \ptr_A -> do
          withArray @_ @e vecx   $ \ptr_x -> do
            withArray @_ @e vecy $ \ptr_y -> do
              case (numericR :: NumericR e) of
                NumericRfloat32 -> C.sgemv C.RowMajor opA' rowsA colsA alpha' ptr_A colsA ptr_x 1 0 ptr_y 1
                NumericRfloat64 -> C.dgemv C.RowMajor opA' rowsA colsA alpha' ptr_A colsA ptr_x 1 0 ptr_y 1
      put future vecy
      return future
