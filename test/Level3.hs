{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Level3 ( tests ) where

import Backend
import Similar

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Data.Complex                           as A
import Data.Array.Accelerate.Numeric.LinearAlgebra.BLAS.Level3

import Hedgehog
import Hedgehog.Gen                                                 ( Gen )
import Hedgehog.Gen.Array
import qualified Hedgehog.Gen                                       as Gen
import qualified Hedgehog.Range                                     as Range

import Data.String
import Text.Printf
import Prelude                                                      as P



tests :: Backend -> IO Bool
tests backend
  = checkParallel
  $ Group (fromString $ printf "Tests.Level3.%s" (show backend))
  [ ("gemm.float32",   test_gemm backend r f32)
  , ("gemm.float64",   test_gemm backend r f64)
  , ("gemm.complex32", test_gemm backend r c32)
  , ("gemm.complex64", test_gemm backend r c64)
  ]
  where
    r   = Range.linearFrom 0 1 64
    f32 = Gen.float  (Range.linearFracFrom 0 (-1) 1)
    f64 = Gen.double (Range.linearFracFrom 0 (-1) 1)
    c32 = (:+) <$> f32 <*> f32
    c64 = (:+) <$> f64 <*> f64

test_gemm
    :: (Numeric e, Similar e)
    => Backend
    -> Range Int
    -> Gen IO e
    -> Property
test_gemm backend r g =
  property $ do
    alpha <- forAll g
    m     <- forAll (Gen.int r)
    n     <- forAll (Gen.int r)
    k     <- forAll (Gen.int r)
    opA   <- forAll (Gen.element [N,T,H])
    opB   <- forAll (Gen.element [N,T,H])
    matA  <- forAll $ case opA of
                        N -> genArray (Z :. m :. k) g
                        _ -> genArray (Z :. k :. m) g
    matB  <- forAll $ case opB of
                        N -> genArray (Z :. k :. n) g
                        _ -> genArray (Z :. n :. k) g
    --
    let test = gemm (constant alpha) opA (use matA) opB (use matB)
    --
    run Interpreter test ~~~ run backend test

