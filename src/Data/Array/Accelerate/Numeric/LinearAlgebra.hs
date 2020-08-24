{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module      : Data.Array.Accelerate.Numeric.LinearAlgebra
-- Copyright   : [2017..2020] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Numeric.LinearAlgebra (

  -- * Types
  Numeric, Scalar, Vector, Matrix,

  -- * Products
  -- ** Vector-vector
  (<.>),
  (><),

  -- ** Matrix-vector
  (#>), (<#),

  -- ** Matrix-matrix
  (<>),

  -- * Diagonal
  identity, diagonal, trace,

) where

import Data.Array.Accelerate                                        as A

import Data.Array.Accelerate.Numeric.LinearAlgebra.Type
import Data.Array.Accelerate.Numeric.LinearAlgebra.BLAS.Level1
import Data.Array.Accelerate.Numeric.LinearAlgebra.BLAS.Level2
import Data.Array.Accelerate.Numeric.LinearAlgebra.BLAS.Level3


-- Level 1
-- -------

-- | An infix synonym for 'dotu'.
--
-- >>> let a = fromList (Z:.4) [1..]
-- >>> let b = fromList (Z:.4) [-2,0,1,1]
-- >>> a <.> b
-- Scalar Z [5.0]
--
-- >>> let c = fromList (Z:.2) [1:+1, 1:+0]
-- >>> let d = fromList (Z:.2) [1:+0, 1:+(-1)]
-- >>> c <.> d
-- Scalar Z [2.0 :+ 0.0]
--
infixr 8 <.>
(<.>) :: Numeric e => Acc (Vector e) -> Acc (Vector e) -> Acc (Scalar e)
(<.>) = dotu


-- | Outer product of two vectors
--
-- >>> let a = fromList (Z :. 3) [1,2,3]
-- >>> let b = fromList (Z :. 3) [5,2,3]
-- >>> a >< b
--  Matrix (Z :. 3 :. 3)
--    [  5.0, 2.0, 3.0
--    , 10.0, 4.0, 6.0
--    , 15.0, 6.0, 9.0 ]
--
infixr 8 ><
(><) :: Numeric e => Acc (Vector e) -> Acc (Vector e) -> Acc (Matrix e)
(><) x y = xc <> yr
  where
    xc = reshape (index2 (length x) 1) x
    yr = reshape (index2 1 (length y)) y


-- Level 2
-- -------

-- | Dense matrix-vector product
--
-- >>> let m = fromList (Z :. 2 :. 3) [1..]
-- >>> m
-- Matrix (Z :. 2 :. 3)
--  [ 1.0, 2.0, 3.0
--  , 4.0, 5.0, 6.0 ]
--
-- >>> let x = fromList (Z :. 3) [10,20,30]
--
-- >>> m #> x
-- Vector (Z :. 2) [140.0,320.0]
--
-- See 'gemv' for a more general version of this operation.
--
infixr 8 #>
(#>) :: Numeric e => Acc (Matrix e) -> Acc (Vector e) -> Acc (Vector e)
(#>) m x = gemv 1 N m x


-- | Dense vector-matrix product
--
-- >>> let m = fromList (Z :. 2 :. 3) [1..]
-- >>> m
-- Matrix (Z :. 2 :. 3)
--  [1.0,2.0,3.0,
--   4.0,5.0,6.0]
--
-- >>> let v = fromList (Z :. 2) [5,10]
--
-- >>> v <# m
-- Vector (Z :. 3) [45.0,60.0,75.0]
--
-- See 'gemv' for a more general version of this operation.
--
infixr 8 <#
(<#) :: Numeric e => Acc (Vector e) -> Acc (Matrix e) -> Acc (Vector e)
(<#) x m = gemv 1 T m x


-- Level 3
-- -------

-- | Dense matrix-matrix product
--
-- >>> let a = fromList (Z :. 3 :. 5) [1..]
-- >>> a
-- Matrix (Z:.3:.5)
--  [  1.0,  2.0,  3.0,  4.0,  5.0
--  ,  6.0,  7.0,  8.0,  9.0, 10.0
--  , 11.0, 12.0, 13.0, 14.0, 15.0 ]
--
-- >>> let b = fromList (Z :. 5 :. 2) [1,3, 0,2, -1,5, 7,7, 6,0]
-- >>> b
-- Matrix (Z :. 5 :. 2)
--  [  1.0, 3.0
--  ,  0.0, 2.0
--  , -1.0, 5.0
--  ,  7.0, 7.0
--  ,  6.0, 0.0 ]
--
-- >>> a <> b
-- Matrix (Z :. 3 :. 2)
--  [  56.0,  50.0
--  , 121.0, 135.0
--  , 186.0, 220.0 ]
--
-- See 'gemm' for a more general version of this operation.
--
infixr 8 <>
(<>) :: Numeric e => Acc (Matrix e) -> Acc (Matrix e) -> Acc (Matrix e)
(<>) matA matB = gemm 1 N matA N matB


-- | Create a square identity matrix of the given dimension
--
identity :: Num e => Exp Int -> Acc (Matrix e)
identity n = diagonal (fill (index1 n) 1)

-- | Create a square matrix with the given diagonal
--
diagonal :: Num e => Acc (Vector e) -> Acc (Matrix e)
diagonal v =
  let n     = length v
      zeros = fill (I2 n n) 0
  in
  permute const zeros (\(I1 i) -> Just_ (I2 i i)) v

-- | The sum of the diagonal elements of a (square) matrix
--
trace :: Num e => Acc (Matrix e) -> Acc (Scalar e)
trace m =
  let Z :. h :. w = unlift (shape m)
  in  sum (backpermute (index1 (min h w)) (\(I1 i) -> I2 i i) m)

