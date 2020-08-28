{-# LANGUAGE MagicHash #-}
-- |
-- Module      : Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Context
-- Copyright   : [2017..2020] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Context (

  withBLAS

) where

import Data.Array.Accelerate.Lifetime
import Data.Array.Accelerate.LLVM.PTX
import Data.Array.Accelerate.LLVM.PTX.Foreign
import Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Base

import Control.Monad.State
import Control.Concurrent.MVar
import Data.IntMap.Strict                                           ( IntMap )
import System.IO.Unsafe
import qualified Data.IntMap.Strict                                 as IM

import qualified Foreign.CUDA.Driver.Context                        as CUDA
import qualified Foreign.CUDA.BLAS                                  as BLAS

import GHC.Ptr
import GHC.Base
import Prelude                                                      hiding ( lookup )


-- Execute an operation with a cuBLAS handle appropriate for the current
-- execution context.
--
-- Initial creation of the context is an atomic operation, but subsequently
-- multiple threads may use the context concurrently.
--
-- <http://docs.nvidia.com/cuda/cublas/index.html#thread-safety2>
--
withBLAS :: (BLAS.Handle -> LLVM PTX b) -> LLVM PTX b
withBLAS k = do
  lc <- gets (deviceContext . ptxContext)
  h  <- liftIO $
          withLifetime lc    $ \ctx ->
          modifyMVar handles $ \im  ->
            let key = toKey ctx in
            case IM.lookup key im of
              -- handle does not exist yet; create it and add to the global
              -- state for reuse
              Nothing -> do
                h <- BLAS.create
                l <- newLifetime h
                -- BLAS.setPointerMode h BLAS.Device
                BLAS.setAtomicsMode h BLAS.Allowed
                addFinalizer lc $ modifyMVar handles (\im' -> return (IM.delete key im', ()))
                addFinalizer l  $ BLAS.destroy h
                return ( IM.insert key l im, l )

              -- return existing handle
              Just h  -> return (im, h)
  --
  withLifetime' h k


toKey :: CUDA.Context -> IM.Key
toKey (CUDA.Context (Ptr addr#)) = I# (addr2Int# addr#)

{-# NOINLINE handles #-}
handles :: MVar (IntMap (Lifetime BLAS.Handle))
handles = unsafePerformIO $ newMVar IM.empty

