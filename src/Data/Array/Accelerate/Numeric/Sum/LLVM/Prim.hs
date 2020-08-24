{-# LANGUAGE CPP          #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : Data.Array.Accelerate.Numeric.Sum.LLVM.Prim
-- Copyright   : [2017..2020] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Numeric.Sum.LLVM.Prim (

  fadd, fsub, fmul,

) where

import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Sugar.Elt

import Data.Array.Accelerate.LLVM.CodeGen.IR                        ( Operands(..), IROP(..) )
import Data.Array.Accelerate.LLVM.CodeGen.Monad                     ( CodeGen, freshName, instr_ )
import Data.Array.Accelerate.LLVM.CodeGen.Sugar                     ( IROpenFun1(..) )

import LLVM.AST.Type.Downcast                                       ( downcast )
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
fadd :: FloatingType a -> IROpenFun1 arch env aenv ((((), EltR a), EltR a) -> EltR a)
fadd = \case
  TypeHalf   -> IRFun1 $ uncurry (binop FAdd TypeHalf)    -- the pattern match yields a ~ EltR a
  TypeFloat  -> IRFun1 $ uncurry (binop FAdd TypeFloat)
  TypeDouble -> IRFun1 $ uncurry (binop FAdd TypeDouble)

-- | As (-), but don't allow potentially unsafe floating-point optimisations.
--
fsub :: FloatingType a -> IROpenFun1 arch env aenv ((((), EltR a), EltR a) -> EltR a)
fsub = \case
  TypeHalf   -> IRFun1 $ uncurry (binop FSub TypeHalf)
  TypeFloat  -> IRFun1 $ uncurry (binop FSub TypeFloat)
  TypeDouble -> IRFun1 $ uncurry (binop FSub TypeDouble)

-- | As (*), but don't allow potentially unsafe floating-point optimisations.
--
fmul :: FloatingType a -> IROpenFun1 arch env aenv ((((), EltR a), EltR a) -> EltR a)
fmul = \case
  TypeHalf   -> IRFun1 $ uncurry (binop FMul TypeHalf)
  TypeFloat  -> IRFun1 $ uncurry (binop FMul TypeFloat)
  TypeDouble -> IRFun1 $ uncurry (binop FMul TypeDouble)

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
upcast _ _ = internalError "upcast" "expected local reference"

