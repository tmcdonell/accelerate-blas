{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Representation.Array                   as Repr
import qualified Data.Array.Accelerate.Array.Unique                 as Unique
import qualified Data.Array.Accelerate.Sugar.Elt                    as Sugar

import Data.Array.Accelerate.Numeric.LinearAlgebra.Type
import Foreign.Ptr                                                  (Ptr)

import qualified Blas.Primitive.Types                               as C

encodeTranspose :: Transpose -> C.Transpose
encodeTranspose N = C.NoTrans
encodeTranspose T = C.Trans
encodeTranspose H = C.ConjTrans

{-# INLINE withArray #-}
withArray
    :: forall sh e b . (Numeric e)
    => Repr.Array sh (Sugar.EltR e)
    -> (Ptr (Sugar.EltR e) -> IO b)
    -> IO b
withArray (Repr.Array _ ad) k =
  -- TODO: explain in comment what this does
  case numericR :: NumericR e of
    NumericRfloat32 -> Unique.withUniqueArrayPtr (ad :: Unique.UniqueArray Float) k
    NumericRfloat64 -> Unique.withUniqueArrayPtr (ad :: Unique.UniqueArray Double) k
    _ -> error "TODO" -- TODO
