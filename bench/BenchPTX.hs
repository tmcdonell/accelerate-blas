-- |
-- Module      : BenchPTX
-- Copyright   : [2017..2020] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module BenchPTX where

import Bench.Accelerate
import Criterion.Main
import Data.Array.Accelerate.LLVM.PTX                               as PTX

main :: IO ()
main = defaultMain (bench_accelerate PTX.runN)

