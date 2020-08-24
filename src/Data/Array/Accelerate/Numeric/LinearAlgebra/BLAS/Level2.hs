{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Module      : Data.Array.Accelerate.Numeric.LinearAlgebra.BLAS.Level2
-- Copyright   : [2017..2020] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Level 2 (matrix-vector) BLAS operations.
--

module Data.Array.Accelerate.Numeric.LinearAlgebra.BLAS.Level2 (

  -- Types
  Numeric, Vector, Matrix, Transpose(..),

  -- Operations
  gemv,

) where

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Data.Complex
import Data.Array.Accelerate.Numeric.LinearAlgebra.Type

#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
import qualified Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.Native.Level2 as CPU
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
import qualified Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Level2    as PTX
#endif


-- | Computes the matrix-vector product of a general matrix.
--
-- \[
-- y = \alpha * \mathrm{op}(A) * x
-- \]
--
-- where:
--
--   * 'shape' \(\mathrm{op}(A)\) @= Z :. m :. n@
--   * 'shape' \(x\) @= Z :. n@
--   * 'shape' \(y\) @= Z :. m@
--
-- <https://software.intel.com/en-us/mkl-developer-reference-c-cblas-gemv>
--
gemv :: forall e. Numeric e
     => Exp e                 -- ^ \( \alpha \)
     -> Transpose             -- ^ Operation to apply to A
     -> Acc (Matrix e)        -- ^ A
     -> Acc (Vector e)        -- ^ x
     -> Acc (Vector e)        -- ^ y
gemv alpha opA matA x = go (lift (unit alpha, matA, x))
  where
    go =
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
      foreignAcc (CPU.gemv nR opA) $
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
      foreignAcc (PTX.gemv nR opA) $
#endif
      (\(T3 _ arr brr) -> mXv arr brr)

    nR = numericR @e

    -- General matrix-vector multiply in pure Accelerate. This is probably not
    -- efficient.
    --
    mXv :: Acc (Matrix e) -> Acc (Vector e) -> Acc (Vector e)
    mXv arr brr
      = fold (+) 0
      $ zipWith (\a b -> alpha * a * b) arr' brr'
      where
        Z :. m :. _ = unlift (shape arr') :: Z :. Exp Int :. Exp Int

        brr' = replicate (lift (Z :. m :. All)) brr
        arr' = case opA of
                  N -> arr
                  T -> transpose arr
                  H -> case nR of
                         NumericRcomplex32 -> map conjugate (transpose arr)
                         NumericRcomplex64 -> map conjugate (transpose arr)
                         _                 -> transpose arr

