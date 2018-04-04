{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Test.BLAS
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Test.BLAS ( testBLAS )
  where

import Test.Util
import Test.BLAS.Level2
import Test.BLAS.Level3

import Test.Tasty


testBLAS :: Run -> TestTree
testBLAS run =
  testGroup "BLAS"
    [ test_level2 run
    , test_level3 run
    ]

