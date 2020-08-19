{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module      : Data.Array.Accelerate.Numeric.Sum.Arithmetic
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Numeric.Sum.Arithmetic (

  fadd, fsub, fmul,

) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Type

import qualified Data.Array.Accelerate.Numeric.Sum.LLVM.Native      as Native
import qualified Data.Array.Accelerate.Numeric.Sum.LLVM.PTX         as PTX
import qualified Data.Array.Accelerate.Sugar.Elt                    as Sugar

-- TODO: NOTE: we don't need the constraint IsFloating a; this is only here
-- until we fix up the PTX.fadd code.
infixl 6 `fadd`
fadd :: (Num a, IsFloating a, IsFloating (Sugar.EltR a)) => Exp a -> Exp a -> Exp a
fadd = Native.fadd $ PTX.fadd (+)

infixl 6 `fsub`
fsub :: (Num a, IsFloating a, IsFloating (Sugar.EltR a)) => Exp a -> Exp a -> Exp a
fsub = Native.fsub $ PTX.fsub (-)

infixl 7 `fmul`
fmul :: (Num a, IsFloating a, IsFloating (Sugar.EltR a)) => Exp a -> Exp a -> Exp a
fmul = Native.fmul $ PTX.fmul (*)

