{-# LANGUAGE RankNTypes #-}
-- |
-- Module      : Test.BLAS.Level3
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Test.BLAS.Level3 ( test_level3 )
  where

import Test.Util                                                    as Gen

import Data.Array.Accelerate.Numeric.LinearAlgebra.BLAS.Level3

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Test.Similar
import qualified Data.Array.Accelerate.Interpreter                  as I

import Hedgehog
import qualified Hedgehog.Gen                                       as Gen
import qualified Hedgehog.Range                                     as Range

import Test.Tasty
import Test.Tasty.Hedgehog


test_gemm
    :: (Show e, Numeric e, Similar e)
    => Run
    -> Range Int
    -> Gen e
    -> Property
test_gemm run r e =
  property $ do
    alpha <- forAll e
    m     <- forAll (Gen.int r)
    n     <- forAll (Gen.int r)
    k     <- forAll (Gen.int r)
    opA   <- forAll (Gen.element [N,T,H])
    opB   <- forAll (Gen.element [N,T,H])
    matA  <- forAll $ case opA of
                        N -> Gen.array (Z :. m :. k) e
                        _ -> Gen.array (Z :. k :. m) e
    matB  <- forAll $ case opB of
                        N -> Gen.array (Z :. k :. n) e
                        _ -> Gen.array (Z :. n :. k) e
    --
    let t = gemm (constant alpha) opA (use matA) opB (use matB)
    --
    I.run t ~~~ run t


test_level3 :: Run -> TestTree
test_level3 run =
  testGroup "Level3"
    [ testGroup "gemm"
      [ testProperty "Float"          $ test_gemm run r f32
      , testProperty "Double"         $ test_gemm run r f64
      , testProperty "Complex Float"  $ test_gemm run r c32
      , testProperty "Complex Double" $ test_gemm run r c64
      ]
    ]
  where
    r   = Range.linearFrom 0 1 64
    f32 = floating :: Gen Float
    f64 = floating :: Gen Double
    c32 = complex f32
    c64 = complex f64

