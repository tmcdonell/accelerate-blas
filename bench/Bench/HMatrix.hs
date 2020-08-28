{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
-- |
-- Module      : Bench.HMatrix
-- Copyright   : [2017..2020] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Bench.HMatrix ( bench_hmatrix )
  where

import Bench.Util

import Control.DeepSeq
import Criterion.Main
import Data.Proxy
import Foreign.Storable
import Numeric.LinearAlgebra                                        hiding ( randomVector )
import Prelude                                                      hiding ( (<>) )
import System.Random.MWC
import Text.Printf


bench_hmatrix :: [Benchmark]
bench_hmatrix =
  [ bench_level2
  , bench_level3
  ]

bench_level2 :: Benchmark
bench_level2 =
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
      let setup :: (Variate e, Storable e) => proxy e -> IO (Matrix e, Vector e)
          setup _ = withSystemRandom $ \gen -> do
            matA <- randomMatrix gen m n
            vecx <- randomVector gen n
            return (matA, vecx)

          go :: (Variate e, Numeric e, NFData e, Show (ArgType e)) => proxy e -> Benchmark
          go t = env (setup t)
               $ \ ~(matA, vecx) -> bench (showType t)
               $ whnf (matA #>) vecx
      in
      bgroup (printf "%dx%d" m n) (sdcz go)

    gevm :: Int -> Int -> Benchmark
    gevm m n =
      let setup :: (Variate e, Storable e) => proxy e -> IO (Matrix e, Vector e)
          setup _ = withSystemRandom $ \gen -> do
            matA <- randomMatrix gen m n
            vecx <- randomVector gen m
            return (matA, vecx)

          go :: (Variate e, Numeric e, NFData e, Show (ArgType e)) => proxy e -> Benchmark
          go t = env (setup t)
               $ \ ~(matA, vecx) -> bench (showType t)
               $ whnf (vecx <#) matA
      in
      bgroup (printf "%dx%d" m n) (sdcz go)

bench_level3 :: Benchmark
bench_level3 =
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
      let
          setup :: (Variate e, Storable e) => proxy e -> IO (Matrix e, Matrix e)
          setup _ = withSystemRandom $ \gen -> do
            matA <- randomMatrix gen m k
            matB <- randomMatrix gen k n
            return (matA, matB)

          go :: (Variate e, Numeric e, NFData e, Show (ArgType e)) => proxy e -> Benchmark
          go t = env (setup t)
               $ \ ~(matA, matB) -> bench (showType t)
               $ whnf (matA <>) matB
      in
      bgroup (printf "%dx%dx%d" m n k) (sdcz go)


randomVector :: (Variate e, Storable e) => GenIO -> Int -> IO (Vector e)
randomVector = uniformVector

randomMatrix :: (Variate e, Storable e) => GenIO -> Int -> Int -> IO (Matrix e)
randomMatrix gen m n = do
  v <- uniformVector gen (m * n)
  return $ reshape n v

sdcz :: (forall (e :: *). (Variate e, Numeric e, NFData e, Show (ArgType e)) => Proxy e -> Benchmark)
     -> [Benchmark]
sdcz go =
  [ go (Proxy :: Proxy Float)
  , go (Proxy :: Proxy Double)
  , go (Proxy :: Proxy (Complex Float))
  , go (Proxy :: Proxy (Complex Double))
  ]

