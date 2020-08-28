-- |
-- Module      : TestNative
-- Copyright   : [2017..2020] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module TestNative where

import Test.BLAS
import Test.Tasty
import Data.Array.Accelerate.LLVM.Native                            as CPU

main :: IO ()
main = defaultMain (testBLAS CPU.run)

