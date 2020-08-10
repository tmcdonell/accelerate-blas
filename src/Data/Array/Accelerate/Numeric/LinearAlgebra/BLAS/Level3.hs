{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE TypeApplications    #-}
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

#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
import qualified Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.Native.Level3 as CPU
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
import qualified Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Level3    as PTX
#endif


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
gemm alpha opA matA opB matB = go (lift (unit alpha, matA, matB))
  where
    go =
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
      foreignAcc (CPU.gemm @e opA opB) $
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
      foreignAcc (PTX.gemm opA opB) $
#endif
      (\(T3 _ arr brr) -> mXm arr brr)

    -- General dense matrix-matrix multiply written in pure Accelerate. This is
    -- not efficient due to the memory access patterns. We could probably
    -- improve this a little bit with a divide-and-conquer algorithm, for
    -- example, but using a foreign implementation will be best.
    --
    mXm :: Acc (Matrix e) -> Acc (Matrix e) -> Acc (Matrix e)
    mXm arr brr
      = fold (+) 0
      $ zipWith (\a b -> alpha * a * b) arrRepl brrRepl
      where
        Z :. rowsA :. _ = unlift (shape arr') :: Z :. Exp Int :. Exp Int
        Z :. colsB :. _ = unlift (shape brr') :: Z :. Exp Int :. Exp Int
        --
        arrRepl         = replicate (lift $ Z :. All   :. colsB :. All) arr'
        brrRepl         = replicate (lift $ Z :. rowsA :. All   :. All) brr'

        -- apply opA
        arr' = case opA of
                 N -> arr
                 T -> transpose arr
                 H -> case numericR :: NumericR e of
                        NumericRcomplex32 -> map conjugate (transpose arr)
                        NumericRcomplex64 -> map conjugate (transpose arr)
                        _                 -> transpose arr

        -- apply opB and transpose at the same time, which is required for this
        -- algorithm
        brr' = case opB of
                 N -> transpose brr
                 T -> brr
                 H -> case numericR :: NumericR e of
                        NumericRcomplex32 -> map conjugate brr
                        NumericRcomplex64 -> map conjugate brr
                        _                 -> brr
