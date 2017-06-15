{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}

module Hedgehog.Gen.Shape where

import Data.Array.Accelerate                              as A

import Hedgehog.Gen                                       ( Gen, int )
import Hedgehog.Range                                     ( Range )


-- Generate a randomly sized shape of the given dimensionality
--
class GenShape sh where
  genShape :: Monad m => Range Int -> Gen m sh

instance GenShape Z where
  genShape _ = return Z

instance GenShape sh => GenShape (sh :. Int) where
  genShape r = (:.) <$> genShape r <*> int r

