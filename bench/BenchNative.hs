-- |
-- Module      : BenchNative
-- Copyright   : [2017..2020] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module BenchNative where

import Bench.Accelerate
import Criterion.Main
import Data.Array.Accelerate.LLVM.Native                            as CPU

main :: IO ()
main = defaultMain (bench_accelerate CPU.runN)

