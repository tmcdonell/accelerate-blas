{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Module      : Data.Array.Accelerate.Numeric.Sum.LLVM.Native
-- Copyright   : [2017..2020] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Numeric.Sum.LLVM.Native (

  fadd, fsub, fmul,

) where

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Type

#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import Data.Array.Accelerate.LLVM.Native.Foreign                    as A
import qualified Data.Array.Accelerate.Numeric.Sum.LLVM.Prim        as Prim
#endif

#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
wrap2 :: (Elt a, Elt b, Elt c)
      => String                                       -- name of the operation
      -> IRFun1 Native () (EltR (a, b) -> EltR c)     -- foreign implementation
      -> (Exp a -> Exp b -> Exp c)                    -- fallback implementation
      -> Exp a
      -> Exp b
      -> Exp c
wrap2 str f g = A.curry (foreignExp (ForeignExp str f) (A.uncurry g))
#endif

fadd :: forall a. (Elt a, IsFloating a) => (Exp a -> Exp a -> Exp a) -> Exp a -> Exp a -> Exp a
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
fadd = wrap2 "fadd" (Prim.fadd (floatingType @a))
#else
fadd = id
#endif

fsub :: forall a. (Elt a, IsFloating a) => (Exp a -> Exp a -> Exp a) -> Exp a -> Exp a -> Exp a
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
fsub = wrap2 "fsub" (Prim.fsub (floatingType @a))
#else
fsub = id
#endif

fmul :: forall a. (Elt a, IsFloating a) => (Exp a -> Exp a -> Exp a) -> Exp a -> Exp a -> Exp a
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
fmul = wrap2 "fmul" (Prim.fmul (floatingType @a))
#else
fmul = id
#endif

