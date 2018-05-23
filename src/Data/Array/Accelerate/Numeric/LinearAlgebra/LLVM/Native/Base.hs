{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Module      : Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.Native.Base
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.Native.Base
  where

import Data.Array.Accelerate.Array.Sugar                            ( Array(..), EltRepr )
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Unique
import Data.Array.Accelerate.Numeric.LinearAlgebra.Type

import qualified Blas.Primitive.Types                               as C


encodeTranspose :: Transpose -> C.Transpose
encodeTranspose N = C.NoTrans
encodeTranspose T = C.Trans
encodeTranspose H = C.ConjTrans


{-# INLINE withArray #-}
withArray
    :: forall sh e b. Numeric e
    => Array sh e
    -> (ArrayPtrs (EltRepr e) -> IO b)
    -> IO b
withArray (Array _ adata) = withArrayData (numericR::NumericR e) adata

{-# INLINE withArrayData #-}
withArrayData
    :: NumericR e
    -> ArrayData (EltRepr e)
    -> (ArrayPtrs (EltRepr e) -> IO b)
    -> IO b
withArrayData NumericRfloat32   (AD_Float  ua)         f = withUniqueArrayPtr ua f
withArrayData NumericRfloat64   (AD_Double ua)         f = withUniqueArrayPtr ua f
withArrayData NumericRcomplex32 (AD_V2 (AD_Float  ua)) f = withUniqueArrayPtr ua f
withArrayData NumericRcomplex64 (AD_V2 (AD_Double ua)) f = withUniqueArrayPtr ua f

