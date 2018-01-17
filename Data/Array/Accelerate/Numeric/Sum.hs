{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
-- |
-- Module      : Data.Array.Accelerate.Numeric.Sum
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Functions for summing floating point numbers more accurately than the
-- straightforward 'Data.Array.Accelerate.sum' operation.
--
-- In the worst case, the 'Data.Array.Accelerate.sum' function accumulates error
-- at a rate proportional to the number of values being summed. The algorithms
-- in this module implement different methods of /compensated summation/, which
-- reduce the accumulation of numeric error so that it grows much more slowly
-- than the number of inputs (e.g. logarithmically), or remains constant.
--

-- TLM: The standard formulation of the algorithms implemented here are not
-- associative; e.g. they would have a type (KBN a -> a -> KBN a). I've
-- done what seems like the sensible conversion, but somebody versed in numeric
-- analysis should probably look...
--
-- See also: <https://hackage.haskell.org/package/math-functions>
--

module Data.Array.Accelerate.Numeric.Sum (

  -- * Summation type class
  Summation(..),
  sum,

  -- * Kahan-Babuška-Neumaier summation
  KBN(..),
  kbn,

  -- * Order-2 Kahan-Babuška summation
  KB2(..),
  kb2,

  -- * Kahan summation
  Kahan(..),
  kahan,

) where

import Data.Array.Accelerate                                        as A hiding ( sum, fromInteger )
import Data.Array.Accelerate.Type                                   as A
import Data.Array.Accelerate.Smart                                  as A ( Exp(..), PreExp(..) )
import Data.Array.Accelerate.Product                                as A
import Data.Array.Accelerate.Array.Sugar                            as A
import Data.Array.Accelerate.Numeric.Sum.Arithmetic                 as A

import Data.Proxy
import Data.Typeable
import Prelude                                                      ( Show, fromInteger )


-- | Sum an array using a particular compensation scheme.
--
-- >>> let xs = [1.0, 1.0e100, 1.0, -1.0e100] :: [Double]
-- >>> Prelude.sum xs
-- 0.0
--
-- >>> let ys = fromList (Z:.4) [1.0, 1.0e100, 1.0, -1.0e100] :: Vector Double
-- >>> sum kbn (use ys)
-- Scalar Z [2.0]
--
sum :: (Summation s a, Shape sh) => Proxy s -> Acc (Array (sh:.Int) a) -> Acc (Array sh a)
sum p = A.map (from p)
      . A.fold add zero
      . A.map (into p)


-- | A class for the summation of floating-point numbers
--
class (Elt a, Elt (s a)) => Summation s a where
  -- | Add a value to the sum
  add  :: Exp (s a) -> Exp (s a) -> Exp (s a)

  -- | The identity of the summation
  zero :: Exp (s a)

  -- | Insert a value into the summation
  into :: Proxy s -> Exp a -> Exp (s a)

  -- | Summarise the result of summation
  from :: Proxy s -> Exp (s a) -> Exp a


-- | Kahan-Babuška-Neumaier summation. This is a little more computationally
-- costly than plain Kahan summation, but is /always/ at least as accurate.
--
data KBN a = KBN a a
  deriving (Show, Typeable)

-- | Return the result of a Kahan-Babuška-Neumaier sum.
--
kbn :: Proxy KBN
kbn = Proxy

kbnAdd :: (Num a, Ord a, IsFloating a) => Exp (KBN a) -> Exp (KBN a) -> Exp (KBN a)
kbnAdd (unlift -> KBN s1 c1) (unlift -> KBN s2 c2) = lift (KBN s' c')
  where
    s' = s1 `fadd` s2
    c' = c1 `fadd` c2 `fadd` if abs s1 >= abs s2
                               then (s1 `fsub` s') `fadd` s2
                               else (s2 `fsub` s') `fadd` s1

-- instance (Num a, Ord a) => Summation KBN a where
--   zero      = lift $ KBN (0::Exp a) (0::Exp a)
--   add       = kbnAdd
--   into _ x  = lift (KBN x 0)
--   from _ x  = let KBN s c = unlift x in s + c

instance Summation KBN Float where
  zero      = constant (KBN 0 0)
  add       = kbnAdd
  into _ x  = lift (KBN x 0)
  from _ x  = let KBN s c = unlift x in s + c

instance Summation KBN Double where
  zero      = constant (KBN 0 0)
  add       = kbnAdd
  into _ x  = lift (KBN x 0)
  from _ x  = let KBN s c = unlift x in s + c

instance Summation KBN CFloat where
  zero      = constant (KBN 0 0)
  add       = kbnAdd
  into _ x  = lift (KBN x 0)
  from _ x  = let KBN s c = unlift x in s + c

instance Summation KBN CDouble where
  zero      = constant (KBN 0 0)
  add       = kbnAdd
  into _ x  = lift (KBN x 0)
  from _ x  = let KBN s c = unlift x in s + c

type instance EltRepr (KBN a) = (((), EltRepr a), EltRepr a)

instance Elt a => Elt (KBN a) where
  eltType _ = UnitTuple `PairTuple` eltType (undefined::a)
                        `PairTuple` eltType (undefined::a)
  toElt (((),a),b)  = KBN (toElt a) (toElt b)
  fromElt (KBN a b) = (((), fromElt a), fromElt b)

instance Elt a => IsProduct Elt (KBN a) where
  type ProdRepr (KBN a) = (((), a), a)
  toProd _ (((),a),b)  = KBN a b
  fromProd _ (KBN a b) = (((),a),b)
  prod _ _             = ProdRsnoc $ ProdRsnoc ProdRunit

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (KBN a) where
  type Plain (KBN a) = KBN (Plain a)
  lift (KBN a b)     = Exp $ Tuple $ NilTup `SnocTup` lift a
                                               `SnocTup` lift b

instance Elt a => Unlift Exp (KBN (Exp a)) where
  unlift t = KBN (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                 (Exp $ ZeroTupIdx `Prj` t)


-- | Second-order Kahan-Babuška summation.  This is more computationally costly
-- than Kahan-Babuška-Neumaier summation. Its advantage is that it can lose less
-- precision (in admittedly obscure cases).
--
-- This method compensates for error in both the sum and the first-order
-- compensation term, hence the use of \"second order\" in the name.
--
data KB2 a = KB2 a a a
  deriving (Show, Typeable)

-- | Return the result of a second-order Kahan-Babuška sum.
--
kb2 :: Proxy KB2
kb2 = Proxy

kb2Add :: (Num a, Ord a, IsFloating a) => Exp (KB2 a) -> Exp (KB2 a) -> Exp (KB2 a)
kb2Add (unlift -> KB2 s1 c1 cc1) (unlift -> KB2 s2 c2 cc2) = lift (KB2 sum' c' cc')
  where
    sum'  = s1 `fadd` s2
    c'    = t  `fadd` k
    cc'   = cc1 `fadd` cc2 `fadd` if abs t >= abs k
                                    then (t `fsub` c') `fadd` k
                                    else (k `fsub` c') `fadd` t
    t     = c1 `fadd` c2
    k     = if abs s1 >= abs s2
              then (s1 `fsub` sum') `fadd` s2
              else (s2 `fsub` sum') `fadd` s1

-- instance (Num a, Ord a) => Summation KB2 a where
--   zero      = lift $ KB2 (0::Exp a) (0::Exp a) (0::Exp a)
--   add       = kb2Add
--   into _ x  = lift (KB2 x 0 0)
--   from _ x  = let KB2 s c cc = unlift x in s + c + cc

instance Summation KB2 Float where
  zero      = constant (KB2 0 0 0)
  add       = kb2Add
  into _ x  = lift (KB2 x 0 0)
  from _ x  = let KB2 s c cc = unlift x in s + c + cc

instance Summation KB2 Double where
  zero      = constant (KB2 0 0 0)
  add       = kb2Add
  into _ x  = lift (KB2 x 0 0)
  from _ x  = let KB2 s c cc = unlift x in s + c + cc

instance Summation KB2 CFloat where
  zero      = constant (KB2 0 0 0)
  add       = kb2Add
  into _ x  = lift (KB2 x 0 0)
  from _ x  = let KB2 s c cc = unlift x in s + c + cc

instance Summation KB2 CDouble where
  zero      = constant (KB2 0 0 0)
  add       = kb2Add
  into _ x  = lift (KB2 x 0 0)
  from _ x  = let KB2 s c cc = unlift x in s + c + cc

type instance EltRepr (KB2 a) = ((((), EltRepr a), EltRepr a), EltRepr a)

instance Elt a => Elt (KB2 a) where
  eltType _ = UnitTuple `PairTuple` eltType (undefined::a)
                        `PairTuple` eltType (undefined::a)
                        `PairTuple` eltType (undefined::a)
  toElt ((((),a),b),c) = KB2 (toElt a) (toElt b) (toElt c)
  fromElt (KB2 a b c)  = ((((), fromElt a), fromElt b), fromElt c)

instance Elt a => IsProduct Elt (KB2 a) where
  type ProdRepr (KB2 a)   = ((((), a), a), a)
  toProd _ ((((),a),b),c) = KB2 a b c
  fromProd _ (KB2 a b c)  = ((((),a),b),c)
  prod _ _                = ProdRsnoc $ ProdRsnoc $ ProdRsnoc ProdRunit

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (KB2 a) where
  type Plain (KB2 a) = KB2 (Plain a)
  lift (KB2 a b c)   = Exp $ Tuple $ NilTup `SnocTup` lift a
                                            `SnocTup` lift b
                                            `SnocTup` lift c

instance Elt a => Unlift Exp (KB2 (Exp a)) where
  unlift t = KB2 (Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t)
                 (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                 (Exp $ ZeroTupIdx `Prj` t)


-- | Kahan summation. This is the least accurate of the compensated summation
-- methods. This summation method is included only for completeness.
--
data Kahan a = Kahan a a
  deriving (Show, Typeable)

-- | Return the result of a Kahan sum.
--
kahan :: Proxy Kahan
kahan = Proxy

kahanAdd :: (Num a, IsFloating a) => Exp (Kahan a) -> Exp (Kahan a) -> Exp (Kahan a)
kahanAdd (unlift -> Kahan s1 c1 :: Kahan (Exp a)) (unlift -> Kahan s2 c2) = lift (Kahan s' c')
  where
    s'  = s1 `fadd` y
    c'  = (s' `fsub` s1) `fsub` y
    y   = s2 `fsub` c1 `fsub` c2

-- instance (Num a, Ord a) => Summation Kahan a where
--   zero      = lift $ Kahan (0::Exp a) (0::Exp a)
--   add       = kahanAdd
--   into _ x  = lift (Kahan x 0)
--   from _ x  = let Kahan s _ = unlift x in s

instance Summation Kahan Float where
  zero      = constant (Kahan 0 0)
  add       = kahanAdd
  into _ x  = lift (Kahan x 0)
  from _ x  = let Kahan s _ = unlift x in s

instance Summation Kahan Double where
  zero      = constant (Kahan 0 0)
  add       = kahanAdd
  into _ x  = lift (Kahan x 0)
  from _ x  = let Kahan s _ = unlift x in s

instance Summation Kahan CFloat where
  zero      = constant (Kahan 0 0)
  add       = kahanAdd
  into _ x  = lift (Kahan x 0)
  from _ x  = let Kahan s _ = unlift x in s

instance Summation Kahan CDouble where
  zero      = constant (Kahan 0 0)
  add       = kahanAdd
  into _ x  = lift (Kahan x 0)
  from _ x  = let Kahan s _ = unlift x in s

type instance EltRepr (Kahan a) = (((), EltRepr a), EltRepr a)

instance Elt a => Elt (Kahan a) where
  eltType _ = UnitTuple `PairTuple` eltType (undefined::a)
                        `PairTuple` eltType (undefined::a)
  toElt (((),a),b)    = Kahan (toElt a) (toElt b)
  fromElt (Kahan a b) = (((), fromElt a), fromElt b)

instance Elt a => IsProduct Elt (Kahan a) where
  type ProdRepr (Kahan a) = (((), a), a)
  toProd _ (((),a),b)     = Kahan a b
  fromProd _ (Kahan a b)  = (((),a),b)
  prod _ _                = ProdRsnoc $ ProdRsnoc ProdRunit

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Kahan a) where
  type Plain (Kahan a) = Kahan (Plain a)
  lift (Kahan a b)     = Exp $ Tuple $ NilTup `SnocTup` lift a
                                              `SnocTup` lift b

instance Elt a => Unlift Exp (Kahan (Exp a)) where
  unlift t = Kahan (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                   (Exp $ ZeroTupIdx `Prj` t)

