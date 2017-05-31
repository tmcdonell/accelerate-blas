{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.Numeric.LinearAlgebra.BLAS.Level3
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Level 3 (matrix-matrix) BLAS operations.
--

module Data.Array.Accelerate.Numeric.LinearAlgebra.BLAS.Level3 (

  -- Types
  Numeric, Matrix, Transpose(..),

  -- Matrix-matrix operations
  gemm,

) where

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Data.Complex                           as A
import Data.Array.Accelerate.Numeric.LinearAlgebra.Type


-- | General matrix-matrix multiply
--
-- \[
-- C = \alpha * \mathrm{op}(A) * \mathrm{op}(B)
-- \]
--
-- where:
--
--   * 'shape' \(\mathrm{op}(A)\) @= Z :. m :. k@
--   * 'shape' \(\mathrm{op}(B)\) @= Z :. k :. n@
--   * 'shape' \(C\) @= Z :. m :. n@
--
-- <https://software.intel.com/en-us/mkl-developer-reference-c-cblas-gemm>
--
gemm :: forall e. Numeric e
     => Exp e                 -- ^ \( \alpha \)
     -> Transpose             -- ^ operation to apply to A
     -> Acc (Matrix e)        -- ^ A
     -> Transpose             -- ^ operation to apply to B
     -> Acc (Matrix e)        -- ^ B
     -> Acc (Matrix e)        -- ^ C
gemm alpha opA matA opB matB =
  matA `mXm` matB
  where
    -- General dense matrix-matrix multiply written in pure Accelerate. This is
    -- not efficient due to the memory access patterns. We could probably
    -- improve this a bit with a divide-and-conquer algorithm, for example.
    --
    mXm :: Acc (Matrix e) -> Acc (Matrix e) -> Acc (Matrix e)
    mXm arr brr
      = fold (+) 0
      $ zipWith (\a b -> alpha * a * b) arrRepl brrRepl
      where
        arr'  = apply  opA arr
        brr'  = apply  opB brr
        brr'T = applyT opB brr

        Z :. rowsA :. _     = unlift (shape arr')  :: Z :. Exp Int :. Exp Int
        Z :. _     :. colsB = unlift (shape brr')  :: Z :. Exp Int :. Exp Int
        --
        arrRepl             = replicate (lift $ Z :. All   :. colsB :. All) arr'
        brrRepl             = replicate (lift $ Z :. rowsA :. All   :. All) brr'T

        apply :: Transpose -> Acc (Matrix e) -> Acc (Matrix e)
        apply op mat = case op of
                         N -> mat
                         T -> transpose mat
                         H -> case numericR :: NumericR e of
                                NumericRcomplex32 -> map conjugate (transpose mat)
                                NumericRcomplex64 -> map conjugate (transpose mat)
                                _                 -> transpose mat

        applyT :: Transpose -> Acc (Matrix e) -> Acc (Matrix e)
        applyT op mat = case op of
                          N -> transpose mat
                          T -> mat
                          H -> case numericR :: NumericR e of
                                 NumericRcomplex32 -> map conjugate mat
                                 NumericRcomplex64 -> map conjugate mat
                                 _                 -> mat

