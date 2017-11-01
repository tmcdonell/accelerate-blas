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

import Data.Array.Accelerate                                        ( Shape, Arrays, Acc )
import Data.Array.Accelerate.Trafo                                  ( Afunction, AfunctionR )
import Data.Array.Accelerate.Array.Sugar                            ( rank )
import Data.Array.Accelerate.Data.Complex
import qualified Data.Array.Accelerate.Hedgehog.Gen.Shape           as Gen

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

shape :: forall sh. (Gen.Shape sh, Shape sh) => Gen sh
shape = Gen.shape (Range.linear 1 (512 `quot` (2 ^ r)))
  where
    r = rank (undefined::sh)

