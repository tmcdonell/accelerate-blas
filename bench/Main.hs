{-# LANGUAGE CPP #-}

module Main where

import HMatrix
import Accelerate

import Data.Array.Accelerate.Debug                                  ( accInit )
import Criterion.Main


main :: IO ()
main = do
  accInit
  defaultMain
    [ benchHMatrix
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
    , benchAcc Native
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
    , benchAcc PTX
#endif
    ]

