-- |
-- Module      : BenchHMatrix
-- Copyright   : [2017..2020] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module BenchHMatrix where

import Bench.HMatrix
import Criterion.Main

main :: IO ()
main = defaultMain bench_hmatrix

