-- |
-- Module      : BenchNative
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module BenchNative where

import Bench.Accelerate
import Criterion.Main
import Data.Array.Accelerate.LLVM.Native                            as CPU

main :: IO ()
main = defaultMain (bench_accelerate CPU.runN)

