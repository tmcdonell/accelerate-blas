{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Test.Util
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Test.Util where

import Data.Array.Accelerate                                        ( Acc, Arrays, Array, Elt, fromList )
import Data.Array.Accelerate.Trafo                                  ( Afunction )
import Data.Array.Accelerate.Trafo.Sharing                          ( AfunctionR )
import Data.Array.Accelerate.Sugar.Shape
import Data.Array.Accelerate.Data.Complex

import Hedgehog
import qualified Hedgehog.Gen                                       as Gen
import qualified Hedgehog.Range                                     as Range
import Prelude                                                      as P


type Run  = forall a. Arrays a => Acc a -> a
type RunN = forall f. Afunction f => f -> AfunctionR f

floating :: P.RealFloat a => Gen a
floating = Gen.realFloat (Range.linearFracFrom 0 (-1) 1)

complex :: Gen a -> Gen (Complex a)
complex f = (:+) <$> f <*> f

array :: (Shape sh, Elt e) => sh -> Gen e -> Gen (Array sh e)
array sh gen = fromList sh <$> Gen.list (Range.singleton (size sh)) gen

