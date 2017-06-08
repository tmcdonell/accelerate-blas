{-# LANGUAGE MagicHash #-}
-- |
-- Module      : Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Context
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Context (

  withBLAS

) where

import Data.Array.Accelerate.Lifetime
import Data.Array.Accelerate.LLVM.PTX
import Data.Array.Accelerate.LLVM.PTX.Foreign

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


withBLAS :: (BLAS.Handle -> IO b) -> LLVM PTX b
withBLAS k = do
  mh <- lookup
  h  <- case mh of
          Nothing -> new
          Just h  -> return h
  liftIO $ withLifetime h k

new :: LLVM PTX (Lifetime BLAS.Handle)
new = do
  lc <- gets (deviceContext . ptxContext)
  liftIO $
    withLifetime lc $ \ctx -> do
      let key = toKey ctx
      h <- BLAS.create
      l <- newLifetime h
      -- BLAS.setPointerMode h BLAS.Device
      BLAS.setAtomicsMode h BLAS.Allowed
      addFinalizer lc $ modifyMVar handles (\im -> return (IM.delete key im, ()))
      addFinalizer l  $ BLAS.destroy h
      modifyMVar handles (\im -> return (IM.insert key l im, l))

lookup :: LLVM PTX (Maybe (Lifetime BLAS.Handle))
lookup = do
  lc <- gets (deviceContext . ptxContext)
  liftIO $
    withLifetime lc  $ \ctx ->
    withMVar handles $ \im  -> return (IM.lookup (toKey ctx) im)

toKey :: CUDA.Context -> IM.Key
toKey (CUDA.Context (Ptr addr#)) = I# (addr2Int# addr#)

{-# NOINLINE handles #-}
handles :: MVar (IntMap (Lifetime BLAS.Handle))
handles = unsafePerformIO $ newMVar IM.empty

