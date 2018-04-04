-- |
-- Module      : BenchHMatrix
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module BenchHMatrix where

import Bench.HMatrix
import Criterion.Main

main :: IO ()
main = defaultMain bench_hmatrix

