{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Data.Array.Accelerate
import Data.Array.Accelerate.Array.Sugar                            ( Array(..) )
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Unique
import Data.Array.Accelerate.Lifetime
import Data.Array.Accelerate.Numeric.LinearAlgebra.Type             as T

import Control.Monad.ST                                             ( RealWorld )

import qualified Data.Vector.Storable.Mutable                       as S
import qualified Numerical.HBLAS.MatrixTypes                        as H


encodeTranspose :: Transpose -> H.Transpose
encodeTranspose N = H.NoTranspose
encodeTranspose T = H.Transpose
encodeTranspose H = H.ConjTranspose


withVector
    :: forall e a. Numeric e
    => Vector e
    -> (H.MDenseVector RealWorld 'H.Direct e -> IO a)
    -> IO a
withVector (Array ((),n) adata) go =
  case (numericR :: NumericR e, adata) of
    (NumericRfloat32, AD_Float  (UniqueArray _ ua)) -> withLifetime ua (\fp -> go (H.MutableDenseVector H.SDirect n 1 (S.MVector n fp)))
    (NumericRfloat64, AD_Double (UniqueArray _ ua)) -> withLifetime ua (\fp -> go (H.MutableDenseVector H.SDirect n 1 (S.MVector n fp)))


withMatrix
    :: forall e a. Numeric e
    => Matrix e
    -> (H.MDenseMatrix RealWorld H.Row e -> IO a)
    -> IO a
withMatrix mat@(Array _ adata) go =
  let Z :. rows :. cols = arrayShape mat
      n                 = rows * cols
  in
  case (numericR :: NumericR e, adata) of
    (NumericRfloat32, AD_Float  (UniqueArray _ ua)) -> withLifetime ua (\fp -> go (H.MutableDenseMatrix H.SRow cols rows cols (S.MVector n fp)))
    (NumericRfloat64, AD_Double (UniqueArray _ ua)) -> withLifetime ua (\fp -> go (H.MutableDenseMatrix H.SRow cols rows cols (S.MVector n fp)))

