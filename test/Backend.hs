{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}

module Backend where

import Data.Array.Accelerate                                        as A
import qualified Data.Array.Accelerate.Interpreter                  as I
#if ACCELERATE_LLVM_NATIVE_BACKEND
import qualified Data.Array.Accelerate.LLVM.Native                  as CPU
#endif
#if ACCELERATE_LLVM_PTX_BACKEND
import qualified Data.Array.Accelerate.LLVM.PTX                     as PTX
#endif

data Backend = Interpreter
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
             | Native
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
             | PTX
#endif

instance Show Backend where
  show Interpreter = "interpreter"
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
  show Native      = "llvm-cpu"
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
  show PTX         = "llvm-ptx"
#endif

{-# INLINE run #-}
run :: Arrays a => Backend -> Acc a -> a
run Interpreter = I.run
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
run Native      = CPU.run
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
run PTX         = PTX.run
#endif


{-# INLINE run1 #-}
run1 :: (Arrays a, Arrays b) => Backend -> (Acc a -> Acc b) -> a -> b
run1 Interpreter f = I.run1 f
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
run1 Native      f = CPU.run1 f
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
run1 PTX         f = PTX.run1 f
#endif

{-# INLINE run2 #-}
run2 :: (Arrays a, Arrays b, Arrays c) => Backend -> (Acc a -> Acc b -> Acc c) -> a -> b -> c
run2 b f x y = go (x,y)
  where
    !go = run1 b (A.uncurry f)

