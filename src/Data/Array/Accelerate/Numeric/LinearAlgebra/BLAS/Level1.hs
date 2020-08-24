{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Module      : Data.Array.Accelerate.Numeric.LinearAlgebra.BLAS.Level1
-- Copyright   : [2017..2020] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Level 1 (vector-vector) BLAS operations.
--

module Data.Array.Accelerate.Numeric.LinearAlgebra.BLAS.Level1 (

  -- Types
  Numeric, Vector,

  -- Level1 operations
  sdot,
  dotu,
  dotc,
  asum,
  amax,
  amin,

) where

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Data.Complex                           as A
import Data.Array.Accelerate.Numeric.LinearAlgebra.Type


-- | Computes a vector-vector dot product, using double precision accumulation
-- of the intermediate result. Includes a scalar (initial) value to be added to
-- the inner product.
--
-- <https://software.intel.com/en-us/mkl-developer-reference-c-cblas-sdot>
--
sdot :: forall e. Numeric e => Exp e -> Acc (Vector e) -> Acc (Vector e) -> Acc (Scalar e)
sdot z xs ys =
  case numericR @e of
    NumericRfloat32   -> map toFloating $ dsdot (toFloating z) (map toFloating xs) (map toFloating ys)
    NumericRfloat64   -> dsdot z xs ys
    NumericRcomplex32 -> map d2f $ zsdot (f2d z) (map f2d xs) (map f2d ys)
    NumericRcomplex64 -> zsdot z xs ys
  where
    dsdot :: Exp Double -> Acc (Vector Double) -> Acc (Vector Double) -> Acc (Scalar Double)
    dsdot z' xs' ys' = fold (+) z' (zipWith (*) xs' ys')

    zsdot :: Exp (Complex Double) -> Acc (Vector (Complex Double)) -> Acc (Vector (Complex Double)) -> Acc (Scalar (Complex Double))
    zsdot z' xs' ys' = fold (+) z' (zipWith (*) xs' ys')

    f2d :: Exp (Complex Float) -> Exp (Complex Double)
    f2d c = lift (toFloating (real c) :+ toFloating (imag c))

    d2f :: Exp (Complex Double) -> Exp (Complex Float)
    d2f c = lift (toFloating (real c) :+ toFloating (imag c))


-- | Computes a vector-vector dot product
--
-- \[
-- res = \sum_i x_i * y_i
-- \]
--
-- <https://software.intel.com/en-us/mkl-developer-reference-c-cblas-dotu>
--
dotu :: Numeric e => Acc (Vector e) -> Acc (Vector e) -> Acc (Scalar e)
dotu xs ys = fold (+) 0 (zipWith (*) xs ys)


-- | Computes a dot product of a conjugated vector with another vector
--
-- \[
-- res = \sum_i \mathrm{conj}(x_i) * y_i
-- \]
--
-- <https://software.intel.com/en-us/mkl-developer-reference-c-cblas-dotc>
--
dotc :: forall e. Numeric (Complex e)
     => Acc (Vector (Complex e))
     -> Acc (Vector (Complex e))
     -> Acc (Scalar (Complex e))
dotc xs ys =
  case numericR @(Complex e) of
    NumericRcomplex32 -> dotu (map conjugate xs) ys
    NumericRcomplex64 -> dotu (map conjugate xs) ys


-- | Computes the sum of magnitudes of the vector elements. For complex values,
-- this is given by \(\sum_i \|\mathrm{real}(x_i)\| + \|\mathrm{imag}(x_i)\|\).
--
-- <https://software.intel.com/en-us/mkl-developer-reference-c-cblas-asum>
--
asum :: forall e. Numeric e => Acc (Vector e) -> Acc (Scalar (NumericBaseT e))
asum =
  case numericR @e of
    NumericRfloat32   -> sum . map abs
    NumericRfloat64   -> sum . map abs
    NumericRcomplex32 -> sum . map mag
    NumericRcomplex64 -> sum . map mag
  where
    mag c = abs (real c) + abs (imag c)


-- | Return the index of the element with the maximum absolute value.
--
-- <https://software.intel.com/en-us/mkl-developer-reference-c-cblas-i-amax>
--
amax :: forall e. Numeric e => Acc (Vector e) -> Acc (Scalar Int)
amax =
  case numericR @e of
    NumericRfloat32   -> map (indexHead . fst) . fold1 cmp . indexed . map abs
    NumericRfloat64   -> map (indexHead . fst) . fold1 cmp . indexed . map abs
    NumericRcomplex32 -> map (indexHead . fst) . fold1 cmp . indexed . map mag
    NumericRcomplex64 -> map (indexHead . fst) . fold1 cmp . indexed . map mag
  where
    cmp ix iy = snd ix > snd iy ? ( ix, iy )
    mag c     = abs (real c) + abs (imag c)

-- | Return the index of the element with the minimum absolute value.
--
-- <https://software.intel.com/en-us/mkl-developer-reference-c-cblas-i-amin>
--
amin :: forall e. Numeric e => Acc (Vector e) -> Acc (Scalar Int)
amin =
  case numericR @e of
    NumericRfloat32   -> map (indexHead . fst) . fold1 cmp . indexed . map abs
    NumericRfloat64   -> map (indexHead . fst) . fold1 cmp . indexed . map abs
    NumericRcomplex32 -> map (indexHead . fst) . fold1 cmp . indexed . map mag
    NumericRcomplex64 -> map (indexHead . fst) . fold1 cmp . indexed . map mag
  where
    cmp ix iy = snd ix < snd iy ? ( ix, iy )
    mag c     = abs (real c) + abs (imag c)

