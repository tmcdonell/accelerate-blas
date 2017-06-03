{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.Native.Level2
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.Native.Level2
  where

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.LLVM.Native.Foreign
import Data.Array.Accelerate.Numeric.LinearAlgebra.Type
import Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.Native.Base

import qualified Numerical.HBLAS.BLAS.Level2                        as H


gemv :: forall e. Numeric e
     => Transpose
     -> ForeignAcc ((Scalar e, Matrix e, Vector e) -> Vector e)
gemv opA = ForeignAcc "cblas.gemv" gemv'
  where
    gemv' (alpha, matA, vecx) = do
      let
          Z :. rowsA :. colsA = arrayShape matA

          opA'    = encodeTranspose opA
          alpha'  = indexArray alpha Z
          leny    = case opA of
                      N -> rowsA
                      _ -> colsA
      --
      vecy  <- allocateRemote (Z :. leny) :: LLVM Native (Vector e)
      liftIO $ do
       withMatrix matA   $ \matA' -> do
        withVector vecx  $ \vecx' -> do
         withVector vecy $ \vecy' -> do
           case numericR :: NumericR e of
             NumericRfloat32  -> H.sgemv opA' alpha' 0 matA' vecx' vecy' >> return vecy
             NumericRfloat64  -> H.dgemv opA' alpha' 0 matA' vecx' vecy' >> return vecy

