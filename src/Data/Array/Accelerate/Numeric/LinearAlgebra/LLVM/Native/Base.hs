{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.Native.Base
-- Copyright   : [2017..2020] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.Native.Base
  where

import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Unique
import Data.Primitive.Vec

import Data.Array.Accelerate.Numeric.LinearAlgebra.Type
import Foreign.Ptr                                                  (Ptr)

import qualified Blas.Primitive.Types                               as C


type family ArrayPtrs e :: *
type instance ArrayPtrs Float         = Ptr Float
type instance ArrayPtrs Double        = Ptr Double
type instance ArrayPtrs (Vec2 Float)  = Ptr Float
type instance ArrayPtrs (Vec2 Double) = Ptr Double

encodeTranspose :: Transpose -> C.Transpose
encodeTranspose N = C.NoTrans
encodeTranspose T = C.Trans
encodeTranspose H = C.ConjTrans

{-# INLINE withArray #-}
withArray
    :: NumericR s e
    -> Array sh e
    -> (ArrayPtrs e -> IO b)
    -> IO b
withArray nR (Array _ ad) k = withArrayData nR ad k

{-# INLINE withArrayData #-}
withArrayData
    :: NumericR s e
    -> ArrayData e
    -> (ArrayPtrs e -> IO b)
    -> IO b
withArrayData NumericRfloat32   = withUniqueArrayPtr
withArrayData NumericRfloat64   = withUniqueArrayPtr
withArrayData NumericRcomplex32 = withUniqueArrayPtr
withArrayData NumericRcomplex64 = withUniqueArrayPtr

