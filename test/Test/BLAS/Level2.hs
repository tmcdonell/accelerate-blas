{-# LANGUAGE RankNTypes #-}
-- |
-- Module      : Test.BLAS.Level2
-- Copyright   : [2017..2020] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Test.BLAS.Level2 ( test_level2 )
  where

import Test.Util                                                    as Gen

import Data.Array.Accelerate.Numeric.LinearAlgebra.BLAS.Level2

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Test.Similar
import qualified Data.Array.Accelerate.Interpreter                  as I

import Hedgehog
import qualified Hedgehog.Gen                                       as Gen
import qualified Hedgehog.Range                                     as Range

import Test.Tasty
import Test.Tasty.Hedgehog


test_gemv
    :: (Show e, Numeric e, Similar e)
    => Run
    -> Range Int
    -> Gen e
    -> Property
test_gemv run r e =
  property $ do
    alpha <- forAll e
    m     <- forAll (Gen.int r)
    n     <- forAll (Gen.int r)
    opA   <- forAll (Gen.element [N,T,H])
    vecx  <- forAll (Gen.array (Z :. n) e)
    matA  <- forAll $ case opA of
                        N -> Gen.array (Z :. m :. n) e
                        _ -> Gen.array (Z :. n :. m) e
    --
    let t = gemv (constant alpha) opA (use matA) (use vecx)
    --
    I.run t ~~~ run t


test_level2 :: Run -> TestTree
test_level2 run =
  testGroup "Level2"
    [ testGroup "gemv"
      [ testProperty "Float"          $ test_gemv run r f32
      , testProperty "Double"         $ test_gemv run r f64
      , testProperty "Complex Float"  $ test_gemv run r c32
      , testProperty "Complex Double" $ test_gemv run r c64
      ]
    ]
  where
    r   = Range.linearFrom 0 1 128
    f32 = floating :: Gen Float
    f64 = floating :: Gen Double
    c32 = complex f32
    c64 = complex f64

