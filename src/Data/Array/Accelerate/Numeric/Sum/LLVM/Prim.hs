{-# LANGUAGE CPP             #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.Numeric.Sum.LLVM.Prim
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Numeric.Sum.LLVM.Prim (

  fadd, fsub, fmul,

) where

import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Error
import qualified Data.Array.Accelerate.Sugar.Elt                    as Sugar

import Data.Array.Accelerate.LLVM.CodeGen.IR                        ( Operands(..), IROP(..) )
import Data.Array.Accelerate.LLVM.CodeGen.Monad                     ( CodeGen, freshName, instr_ )
import Data.Array.Accelerate.LLVM.CodeGen.Sugar                     ( IROpenFun1(..) )

import LLVM.AST.Type.Downcast                                       ( downcast )
import qualified Data.Array.Accelerate.LLVM.CodeGen.Arithmetic      as A
import qualified LLVM.AST.Type.Name                                 as A
import qualified LLVM.AST.Type.Operand                              as A
import qualified LLVM.AST.Type.Representation                       as A

import LLVM.AST.Instruction
import LLVM.AST.Name                                                ( Name(..) )
import LLVM.AST.Operand                                             ( Operand(..) )
import LLVM.AST.Type                                                ( Type(..), FloatingPointType(..) )

import Prelude                                                      hiding (uncurry)

uncurry :: (Operands a -> Operands b -> c) -> Operands (((), a), b) -> c
uncurry f (OP_Unit `OP_Pair` x `OP_Pair` y) = f x y

-- | As (+), but don't allow potentially unsafe floating-point optimisations.
--
fadd :: FloatingType (Sugar.EltR a) -> IROpenFun1 arch env aenv (Sugar.EltR (a,a) -> Sugar.EltR a)
fadd t = IRFun1 $ uncurry (binop FAdd t)

-- | As (-), but don't allow potentially unsafe floating-point optimisations.
--
fsub :: FloatingType (Sugar.EltR a) -> IROpenFun1 arch env aenv (Sugar.EltR (a,a) -> Sugar.EltR a)
fsub t = IRFun1 $ uncurry (binop FSub t)

-- | As (*), but don't allow potentially unsafe floating-point optimisations.
--
fmul :: FloatingType (Sugar.EltR a) -> IROpenFun1 arch env aenv (Sugar.EltR (a,a) -> Sugar.EltR a)
fmul t = IRFun1 $ uncurry (binop FMul t)

-- use of 'op' mandates that t ~ u for (FloatingType t) and (Operands u)- how to
-- fix?
binop :: (FastMathFlags -> Operand -> Operand -> InstructionMetadata -> Instruction)
      -> FloatingType a
      -> Operands a
      -> Operands a
      -> CodeGen arch (Operands a)
binop f t (op t -> x) (op t -> y) = do
  r <- instr (downcast t) (f fmf (downcast x) (downcast y) md)
  return (upcast t r)


-- Prim
-- ----

md :: InstructionMetadata
md = []

fmf :: FastMathFlags
fmf = noFastMathFlags

fresh :: CodeGen arch Name
fresh = downcast <$> freshName

instr :: Type -> Instruction -> CodeGen arch Operand
instr ty ins = do
  name <- fresh
  instr_ (name := ins)
  return (LocalReference ty name)

upcast :: FloatingType t -> Operand -> Operands t
upcast TypeHalf{}    (LocalReference (FloatingPointType HalfFP)   (UnName x)) = OP_Half    (A.LocalReference A.type' (A.UnName x))
upcast TypeFloat{}   (LocalReference (FloatingPointType FloatFP)  (UnName x)) = OP_Float   (A.LocalReference A.type' (A.UnName x))
upcast TypeDouble{}  (LocalReference (FloatingPointType DoubleFP) (UnName x)) = OP_Double  (A.LocalReference A.type' (A.UnName x))
-- TODO: is this supposed to be at compile-time?
upcast _ _ = internalError "upcast" "expected local reference"
