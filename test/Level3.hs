{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Level3 ( tests ) where

import Backend
import Similar

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Data.Complex                           as A
import Data.Array.Accelerate.Numeric.LinearAlgebra

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
  let !expect = run1 Interpreter (A.uncurry (<>)) -- so slooooow D:
      !actual = run1 backend     (A.uncurry (<>))
  in
  property $ do
    m     <- forAll (Gen.int r)
    n     <- forAll (Gen.int r)
    k     <- forAll (Gen.int r)
    matA  <- forAll (genArray (Z :. m :. k) g)
    matB  <- forAll (genArray (Z :. k :. n) g)
    expect (matA,matB) ~~~ actual (matA, matB)

