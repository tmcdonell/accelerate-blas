{-# LANGUAGE CPP #-}
-- |
-- Module      : Data.Array.Accelerate.Numeric.Sum.LLVM.PTX
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Numeric.Sum.LLVM.PTX (

  fadd, fsub, fmul,

) where

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Type

#ifdef ACCELERATE_LLVM_PTX_BACKEND
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import Data.Array.Accelerate.LLVM.PTX.Foreign                       as A
import qualified Data.Array.Accelerate.Numeric.Sum.LLVM.Prim        as Prim
#endif

#ifdef ACCELERATE_LLVM_PTX_BACKEND
wrap2 :: (Elt a, Elt b, Elt c)
      => String                                       -- name of the operation
      -> IRFun1 PTX () ((a, b) -> c)                  -- foreign implementation
      -> (Exp a -> Exp b -> Exp c)                    -- fallback implementation
      -> Exp a
      -> Exp b
      -> Exp c
wrap2 str f g = A.curry (foreignExp (ForeignExp str f) (A.uncurry g))
#endif

fadd :: (IsFloating a, Elt a) => (Exp a -> Exp a -> Exp a) -> Exp a -> Exp a -> Exp a
#ifdef ACCELERATE_LLVM_PTX_BACKEND
fadd = wrap2 "fadd" (Prim.fadd floatingType)
#else
fadd = id
#endif

fsub :: (IsFloating a, Elt a) => (Exp a -> Exp a -> Exp a) -> Exp a -> Exp a -> Exp a
#ifdef ACCELERATE_LLVM_PTX_BACKEND
fsub = wrap2 "fsub" (Prim.fsub floatingType)
#else
fsub = id
#endif

fmul :: (IsFloating a, Elt a) => (Exp a -> Exp a -> Exp a) -> Exp a -> Exp a -> Exp a
#ifdef ACCELERATE_LLVM_PTX_BACKEND
fmul = wrap2 "fmul" (Prim.fmul floatingType)
#else
fmul = id
#endif

