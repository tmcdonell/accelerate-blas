-- |
-- Module      : BenchPTX
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module BenchPTX where

import Bench.Accelerate
import Criterion.Main
import Data.Array.Accelerate.LLVM.PTX                               as PTX

main :: IO ()
main = defaultMain (bench_accelerate PTX.runN)

