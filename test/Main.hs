{-# LANGUAGE CPP #-}

module Main where

import Backend

import System.IO
import qualified Level3

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  sequence_
    [ return True
#if ACCELERATE_LLVM_NATIVE_BACKEND
    , Level3.tests Native
#endif
#if ACCELERATE_LLVM_PTX_BACKEND
    , Level3.tests PTX
#endif
    ]

