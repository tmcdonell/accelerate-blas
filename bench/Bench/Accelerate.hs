{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
-- |
-- Module      : Bench.Accelerate
-- Copyright   : [2017..2020] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Bench.Accelerate ( bench_accelerate )
  where

import Bench.Util

import Data.Array.Accelerate                                        ( Elt, Z(..), (:.)(..) )
import Data.Array.Accelerate.Trafo                                  ( Afunction )
import Data.Array.Accelerate.Trafo.Sharing                          ( AfunctionR )
import Data.Array.Accelerate.Numeric.LinearAlgebra
import Data.Array.Accelerate.Data.Complex
import Data.Array.Accelerate.System.Random.MWC

import Criterion.Main
import Data.Proxy
import Text.Printf
import Prelude                                                      hiding ( (<>) )


type RunN = forall f. Afunction f => f -> AfunctionR f

bench_accelerate :: RunN -> [Benchmark]
bench_accelerate runN =
  [ bench_level2 runN
  , bench_level3 runN
  ]


bench_level2 :: RunN -> Benchmark
bench_level2 runN =
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
      let setup :: (Variate e, Elt e) => proxy e -> IO (Matrix e, Vector e)
          setup _ = withSystemRandom $ \gen -> do
            matA <- randomArrayWith gen uniform (Z :. m :. n)
            vecx <- randomArrayWith gen uniform (Z :. n)
            return (matA, vecx)

          go :: (Variate e, Numeric e, Show (ArgType e)) => proxy e -> Benchmark
          go t = env (setup t)
               $ \ ~(matA, vecx) -> bench (showType t)
               $ whnf (runN (#>) matA) vecx
      in
      bgroup (printf "%dx%d" m n) (sdcz go)

    gevm :: Int -> Int -> Benchmark
    gevm m n =
      let setup :: (Variate e, Elt e) => proxy e -> IO (Matrix e, Vector e)
          setup _ = withSystemRandom $ \gen -> do
            matA <- randomArrayWith gen uniform (Z :. m :. n)
            vecx <- randomArrayWith gen uniform (Z :. m)
            return (matA, vecx)

          go :: (Variate e, Numeric e, Show (ArgType e)) => proxy e -> Benchmark
          go t = env (setup t)
               $ \ ~(matA, vecx) -> bench (showType t)
               $ whnf (runN (<#) vecx) matA
      in
      bgroup (printf "%dx%d" m n) (sdcz go)


bench_level3 :: RunN -> Benchmark
bench_level3 runN =
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
      let setup :: (Variate e, Elt e) => proxy e -> IO (Matrix e, Matrix e)
          setup _ = withSystemRandom $ \gen -> do
            matA <- randomArrayWith gen uniform (Z :. m :. k)
            matB <- randomArrayWith gen uniform (Z :. k :. n)
            return (matA, matB)

          go :: (Variate e, Numeric e, Show (ArgType e)) => proxy e -> Benchmark
          go t = env (setup t)
               $ \ ~(matA, matB) -> bench (showType t)
               $ whnf (runN (<>) matA) matB
      in
      bgroup (printf "%dx%dx%d" m n k) (sdcz go)

sdcz :: (forall (e :: *). (Variate e, Numeric e, Show (ArgType e)) => Proxy e -> Benchmark)
     -> [Benchmark]
sdcz go =
  [ go (Proxy :: Proxy Float)
  , go (Proxy :: Proxy Double)
  , go (Proxy :: Proxy (Complex Float))
  , go (Proxy :: Proxy (Complex Double))
  ]

