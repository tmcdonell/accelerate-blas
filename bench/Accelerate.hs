{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}

module Accelerate (

  Backend(..),
  benchAcc,

) where

import Extra

import Data.Array.Accelerate                                        ( Acc, Arrays, Elt, Z(..), (:.)(..) )
import Data.Array.Accelerate.Numeric.LinearAlgebra
import Data.Array.Accelerate.Data.Complex
import Data.Array.Accelerate.System.Random.MWC
import qualified Data.Array.Accelerate                              as A
import qualified Data.Array.Accelerate.Interpreter                  as I
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
import qualified Data.Array.Accelerate.LLVM.Native                  as CPU
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
import qualified Data.Array.Accelerate.LLVM.PTX                     as PTX
#endif

import Criterion.Main
import Data.Proxy
import Text.Printf


benchAcc :: Backend -> Benchmark
benchAcc backend =
  bgroup (show backend)
    [ level2 backend
    , level3 backend
    ]


level2 :: Backend -> Benchmark
level2 backend =
  bgroup "matrix-vector"
    [ bgroup "(#>)"
      [ gemv  200  400
      , gemv  500 1000
      , gemv 1000 2000
      , gemv 2000 3000
      ]
    , bgroup "(<#)"
      [ gevm  200  400
      , gevm  500 1000
      , gevm 1000 2000
      , gevm 2000 3000
      ]
    ]
  where
    gemv :: Int -> Int -> Benchmark
    gemv m n =
      let complexity = m * n

          setup :: (Variate e, Elt e) => proxy e -> IO (Matrix e, Vector e)
          setup _ = withSystemRandom $ \gen -> do
            matA <- randomArrayWith gen uniform (Z :. m :. n)
            vecx <- randomArrayWith gen uniform (Z :. n)
            return (matA, vecx)

          go :: (Variate e, Numeric e, Show (ArgType e)) => proxy e -> Benchmark
          go t = env (setup t)
               $ \ ~(matA, vecx) -> bench (showType t)
               $ whnf (run2 backend (#>) matA) vecx
      in
      bgroup (printf "%dx%d" m n) (sdcz go complexity backend)

    gevm :: Int -> Int -> Benchmark
    gevm m n =
      let complexity = m * n

          setup :: (Variate e, Elt e) => proxy e -> IO (Matrix e, Vector e)
          setup _ = withSystemRandom $ \gen -> do
            matA <- randomArrayWith gen uniform (Z :. m :. n)
            vecx <- randomArrayWith gen uniform (Z :. m)
            return (matA, vecx)

          go :: (Variate e, Numeric e, Show (ArgType e)) => proxy e -> Benchmark
          go t = env (setup t)
               $ \ ~(matA, vecx) -> bench (showType t)
               $ whnf (run2 backend (<#) vecx) matA
      in
      bgroup (printf "%dx%d" m n) (sdcz go complexity backend)


level3 :: Backend -> Benchmark
level3 backend =
  bgroup "matrix-matrix"
    [ bgroup "(<>)"
      [ gemm  100  100  100
      , gemm  250  250  250
      , gemm  500  500  500
      , gemm 1000 1000 1000
      ]
    ]
  where
    gemm :: Int -> Int -> Int -> Benchmark
    gemm m n k =
      let complexity = m * n * k

          setup :: (Variate e, Elt e) => proxy e -> IO (Matrix e, Matrix e)
          setup _ = withSystemRandom $ \gen -> do
            matA <- randomArrayWith gen uniform (Z :. m :. k)
            matB <- randomArrayWith gen uniform (Z :. k :. n)
            return (matA, matB)

          go :: (Variate e, Numeric e, Show (ArgType e)) => proxy e -> Benchmark
          go t = env (setup t)
               $ \ ~(matA, matB) -> bench (showType t)
               $ whnf (run2 backend (<>) matA) matB
      in
      bgroup (printf "%dx%dx%d" m n k) (sdcz go complexity backend)


sdcz :: (forall (e :: *). (Variate e, Numeric e, Show (ArgType e)) => Proxy e -> Benchmark)
     -> Int
     -> Backend
     -> [Benchmark]
sdcz go complexity backend =
  if maybe True (complexity <=) (complexityLimit backend)
    then
      [ go (Proxy :: Proxy Float)
      , go (Proxy :: Proxy Double)
      , go (Proxy :: Proxy (Complex Float))
      , go (Proxy :: Proxy (Complex Double))
      ]
    else
      []

complexityLimit :: Backend -> Maybe Int
complexityLimit Interpreter = Just 50000
complexityLimit _           = Nothing


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

