{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators     #-}

module Similar where

import Data.Complex
import Data.Array.Accelerate                              ( Array, Shape, Z, (:.)(..), arrayShape, toList )

import Hedgehog
import GHC.Stack


infix 4 ~~~
(~~~) :: (MonadTest m, Similar a, Show (Sim a), HasCallStack) => a -> a -> m ()
a ~~~ b = withFrozenCallStack $ Sim a === Sim b


data Sim a = Sim a

instance Similar a => Eq (Sim a) where
  Sim a == Sim b = a ~= b

instance Show a => Show (Sim a) where
  show (Sim a) = show a


-- A class of things that support almost-equality, so that we can disregard
-- small amounts of floating-point round-off error.
--
class Similar a where
  {-# INLINE (~=) #-}
  (~=) :: a -> a -> Bool
  default (~=) :: Eq a => a -> a -> Bool
  (~=) = (==)

infix 4 ~=

instance Similar Float   where (~=) = absRelTol 0.00005 0.005
instance Similar Double  where (~=) = absRelTol 0.00005 0.005

instance Similar e => Similar (Complex e) where
  (r1 :+ i1) ~= (r2 :+ i2) = r1 ~= r2 && i1 ~= i2

instance Similar Z
instance (Eq sh, Eq sz) => Similar (sh:.sz)

instance Similar a => Similar [a] where
  []     ~= []          = True
  (x:xs) ~= (y:ys)      = x ~= y && xs ~= ys
  _      ~= _           = False

instance (Similar e, Eq sh, Shape sh) => Similar (Array sh e) where
  a1 ~= a2      =  arrayShape a1 == arrayShape a2
                && toList a1     ~= toList a2


{-# INLINEABLE absRelTol #-}
absRelTol :: RealFloat a => a -> a -> a -> a -> Bool
absRelTol epsilonAbs epsilonRel u v
  |  isInfinite u
  && isInfinite v          = True
  |  isNaN u
  && isNaN v               = True
  | abs (u-v) < epsilonAbs = True
  | abs u > abs v          = abs ((u-v) / u) < epsilonRel
  | otherwise              = abs ((v-u) / v) < epsilonRel

