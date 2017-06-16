{-# LANGUAGE CPP #-}

module Main where

import Backend

import System.IO
import qualified Level3
import qualified Level2

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  sequence_
    [ return True
#if ACCELERATE_LLVM_NATIVE_BACKEND
    , Level2.tests Native
    , Level3.tests Native
#endif
#if ACCELERATE_LLVM_PTX_BACKEND
    , Level2.tests PTX
    , Level3.tests PTX
#endif
    ]

