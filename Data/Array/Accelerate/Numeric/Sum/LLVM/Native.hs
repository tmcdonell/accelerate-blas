{-# LANGUAGE CPP #-}
-- |
-- Module      : Data.Array.Accelerate.Numeric.Sum.LLVM.Native
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Numeric.Sum.LLVM.Native (

  fadd, fsub, fmul,

) where

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Type

#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import Data.Array.Accelerate.LLVM.Native.Foreign                    as A
import qualified Data.Array.Accelerate.Numeric.Sum.LLVM.Prim        as Prim
#endif

#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
wrap2 :: (Elt a, Elt b, Elt c)
      => String                                       -- name of the operation
      -> IRFun1 Native () ((a, b) -> c)               -- foreign implementation
      -> (Exp a -> Exp b -> Exp c)                    -- fallback implementation
      -> Exp a
      -> Exp b
      -> Exp c
wrap2 str f g = A.curry (foreignExp (ForeignExp str f) (A.uncurry g))
#endif

fadd :: (IsFloating a, Elt a) => (Exp a -> Exp a -> Exp a) -> Exp a -> Exp a -> Exp a
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
fadd = wrap2 "fadd" (Prim.fadd floatingType)
#else
fadd = id
#endif

fsub :: (IsFloating a, Elt a) => (Exp a -> Exp a -> Exp a) -> Exp a -> Exp a -> Exp a
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
fsub = wrap2 "fsub" (Prim.fsub floatingType)
#else
fsub = id
#endif

fmul :: (IsFloating a, Elt a) => (Exp a -> Exp a -> Exp a) -> Exp a -> Exp a -> Exp a
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
fmul = wrap2 "fmul" (Prim.fmul floatingType)
#else
fmul = id
#endif

