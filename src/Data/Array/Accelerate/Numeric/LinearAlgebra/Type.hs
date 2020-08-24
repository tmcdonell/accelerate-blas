{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
-- |
-- Module      : Data.Array.Accelerate.Numeric.LinearAlgebra.Type
-- Copyright   : [2017..2020] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Numeric.LinearAlgebra.Type (

  module Data.Array.Accelerate.Numeric.LinearAlgebra.Type,

) where

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Data.Complex                           as A
import Data.Array.Accelerate.Sugar.Elt
import Data.Primitive.Vec

import qualified Prelude                                            as P


-- For explicit dictionary reification, to recover the type the operation should
-- be performed at.
--
data NumericR s r where
  NumericRfloat32   :: NumericR Float Float
  NumericRfloat64   :: NumericR Double Double
  NumericRcomplex32 :: NumericR (Complex Float) (Vec2 Float)
  NumericRcomplex64 :: NumericR (Complex Double) (Vec2 Double)

class Num a => Numeric a where
  numericR :: NumericR a (EltR a)

instance Numeric Float where
  numericR = NumericRfloat32

instance Numeric Double where
  numericR = NumericRfloat64

instance Numeric (Complex Float) where
  numericR = NumericRcomplex32

instance Numeric (Complex Double) where
  numericR = NumericRcomplex64

-- class Numeric a => RealNumeric a
--
-- instance RealNumeric Float
-- instance RealNumeric Double

type family NumericBaseT t where
  NumericBaseT Float            = Float
  NumericBaseT Double           = Double
  NumericBaseT (Complex Float)  = Float
  NumericBaseT (Complex Double) = Double


-- | Orientation of the underlying data.
--
-- Accelerate arrays are naturally stored in row-major format.
--
data Orientation
  = R -- ^ row major
  | C -- ^ column major
  deriving (P.Eq, P.Show)

-- | Many operations allow you to implicitly transpose the arguments. For
-- a given input matrix @mat@ with dimensions @Z :. m :. n@ (that is; @m@ rows
-- and @n@ columns):
--
data Transpose
  -- | Leave the matrix as is.
  = N

  -- | Treat the matrix as implicitly transposed, with dimensions @Z :. n :. m@.
  -- Entry @Z :. j :. i@ is treated as actually being entry @Z :. i :. j@.
  | T

  -- | Implicitly transpose and conjugate the input matrix. For complex-valued
  -- matrices a given element @mat ! Z:.j:.i == x :+ y@ will be treated as
  -- actually being @mat ! Z:.i:.j == x :+ (-y)@.
  | H
  deriving (P.Eq, P.Show)

