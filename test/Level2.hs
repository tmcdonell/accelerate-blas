{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Level2 ( tests ) where

import Backend
import Similar

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Data.Complex                           as A
import Data.Array.Accelerate.Numeric.LinearAlgebra.BLAS.Level2

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
  $ Group (fromString $ printf "Tests.Level2.%s" (show backend))
  [ ("gemv.float32",   test_gemv backend r f32)
  , ("gemv.float64",   test_gemv backend r f64)
  , ("gemv.complex32", test_gemv backend r c32)
  , ("gemv.complex64", test_gemv backend r c64)
  ]
  where
    r   = Range.linearFrom 0 1 128
    f32 = Gen.float  (Range.linearFracFrom 0 (-1) 1)
    f64 = Gen.double (Range.linearFracFrom 0 (-1) 1)
    c32 = (:+) <$> f32 <*> f32
    c64 = (:+) <$> f64 <*> f64

test_gemv
    :: (Numeric e, Similar e)
    => Backend
    -> Range Int
    -> Gen IO e
    -> Property
test_gemv backend r g =
  property $ do
    alpha <- forAll g
    m     <- forAll (Gen.int r)
    n     <- forAll (Gen.int r)
    opA   <- forAll (Gen.element [N,T,H])
    vecx  <- forAll (genArray (Z :. n) g)
    matA  <- forAll $ case opA of
                        N -> genArray (Z :. m :. n) g
                        _ -> genArray (Z :. n :. m) g
    --
    let test = gemv (constant alpha) opA (use matA) (use vecx)
    --
    run Interpreter test ~~~ run backend test



