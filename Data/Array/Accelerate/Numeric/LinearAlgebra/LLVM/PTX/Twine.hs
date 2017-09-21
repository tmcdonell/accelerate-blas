{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
-- |
-- Module      : Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Twine
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Twine (

  interleave,
  deinterleave,

) where

import Data.Array.Accelerate.Data.Complex
import Data.Array.Accelerate.Array.Sugar                            ( EltRepr, Vector, Z(..), (:.)(..) )
import Data.Array.Accelerate.Lifetime
import Data.Array.Accelerate.LLVM.PTX
import Data.Array.Accelerate.LLVM.PTX.Foreign

import Data.Array.Accelerate.Numeric.LinearAlgebra.Type
import Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.PTX.Base

import Control.Concurrent.MVar
import Control.Monad.State
import Data.ByteString                                              ( ByteString )
import Data.FileEmbed
import Data.IntMap.Strict                                           ( IntMap )
import Foreign.Storable.Complex                                     ( )
import System.IO.Unsafe
import qualified Data.IntMap.Strict                                 as IM

import Foreign.CUDA.Ptr                                             ( DevicePtr )
import Foreign.CUDA.Analysis
import qualified Foreign.CUDA.Driver                                as CUDA
import qualified Foreign.CUDA.Driver.Stream                         as CUDA

import GHC.Ptr
import GHC.Base
import Prelude                                                      hiding ( lookup )


interleave
    :: forall e b. Numeric (Complex e)
    => DevicePtrs (EltRepr (Complex e))
    -> Stream
    -> Int
    -> (DevicePtr (Complex e) -> LLVM PTX b)  -- device pointer is in packed representation
    -> LLVM PTX b
interleave (((), d_re), d_im) s n k = do
  case numericR :: NumericR (Complex e) of
    nR@NumericRcomplex32 -> do
      cplx <- allocateRemote (Z :. n * 2) :: LLVM PTX (Vector Float)
      withTwine nR        $ \(_,pack,_) -> do
        withArray cplx s  $ \d_cplx     -> do
          withLifetime' s $ \s'         -> do
            liftIO $ launch pack s' n d_cplx d_re d_im
          k (CUDA.castDevPtr d_cplx :: DevicePtr (Complex Float))
    --
    nR@NumericRcomplex64 -> do
      cplx <- allocateRemote (Z :. n * 2) :: LLVM PTX (Vector Double)
      withTwine nR        $ \(_,pack,_) -> do
        withArray cplx s  $ \d_cplx     -> do
          withLifetime' s $ \s'         -> do
            liftIO $ launch pack s' n d_cplx d_re d_im
          k (CUDA.castDevPtr d_cplx :: DevicePtr (Complex Double))

deinterleave
    :: forall e. Numeric (Complex e)
    => DevicePtrs (EltRepr (Complex e))
    -> DevicePtr (Complex e)  -- in packed representation
    -> Stream
    -> Int
    -> LLVM PTX ()
deinterleave (((), d_re), d_im) d_cplx s n = do
  case numericR :: NumericR (Complex e) of
    nR@NumericRcomplex32 -> do
      withTwine nR      $ \(_,_,unpack) -> do
        withLifetime' s $ \s'           -> do
          liftIO $ launch unpack s' n d_re d_im (CUDA.castDevPtr d_cplx :: DevicePtr Float)
    --
    nR@NumericRcomplex64 -> do
      withTwine nR      $ \(_,_,unpack) -> do
        withLifetime' s $ \s'           -> do
          liftIO $ launch unpack s' n d_re d_im (CUDA.castDevPtr d_cplx :: DevicePtr Double)


withTwine :: NumericR (Complex e) -> ((CUDA.Module, Kernel, Kernel) -> LLVM PTX b) -> LLVM PTX b
withTwine nR k = do
  ptx <- gets ptxContext
  let lc  = deviceContext ptx
      prp = deviceProperties ptx
      mds = modules nR
  --
  mdl <- liftIO $ do
    withLifetime lc $ \ctx -> do
     modifyMVar mds $ \im  -> do
      let key = toKey ctx
      case IM.lookup key im of
        -- Module is not loaded yet; add to the current context and the global
        -- state for later reuse
        Nothing -> do
          mdl     <- CUDA.loadData $ case nR of
                                       NumericRcomplex32 -> ptx_twine_f32
                                       NumericRcomplex64 -> ptx_twine_f64
          pack    <- mkKernel "interleave"   mdl prp
          unpack  <- mkKernel "deinterleave" mdl prp
          let mkk = (mdl, pack, unpack)
          --
          lm      <- newLifetime mkk
          addFinalizer lc $ modifyMVar mds (\im' -> return (IM.delete key im', ()))
          addFinalizer lm $ CUDA.unload mdl
          return ( IM.insert key lm im, lm )

        -- Return existing module
        Just lm  -> return (im, lm)
  --
  withLifetime' mdl k


toKey :: CUDA.Context -> IM.Key
toKey (CUDA.Context (Ptr addr#)) = I# (addr2Int# addr#)


launch :: Kernel -> CUDA.Stream -> Int -> DevicePtr e -> DevicePtr e -> DevicePtr e -> IO ()
launch Kernel{..} s n dx dy dz =
  CUDA.launchKernel kernelFun (kernelThreadBlocks n,1,1) (kernelThreadBlockSize,1,1) kernelSharedMemBytes (Just s)
    [ CUDA.VArg dx, CUDA.VArg dy, CUDA.VArg dz, CUDA.IArg (fromIntegral n) ]

mkKernel :: String -> CUDA.Module -> CUDA.DeviceProperties -> IO Kernel
mkKernel name mdl prp = do
  fun <- CUDA.getFun mdl name
  reg <- CUDA.requires fun CUDA.NumRegs
  let
      blockSize   = 256
      sharedMem   = 0
      maxBlocks   = maxResidentBlocks prp blockSize reg sharedMem
      numBlocks n = maxBlocks `min` ((n + blockSize - 1) `quot` blockSize)
  --
  return $ Kernel fun sharedMem blockSize numBlocks name

data Kernel = Kernel {
    kernelFun               :: {-# UNPACK #-} !CUDA.Fun
  , kernelSharedMemBytes    :: {-# UNPACK #-} !Int
  , kernelThreadBlockSize   :: {-# UNPACK #-} !Int
  , kernelThreadBlocks      :: (Int -> Int)
  , kernelName              :: String
  }

modules :: NumericR (Complex e) -> MVar (IntMap (Lifetime (CUDA.Module, Kernel, Kernel)))
modules NumericRcomplex32 = modules_f32
modules NumericRcomplex64 = modules_f64

{-# NOINLINE modules_f32 #-}
modules_f32 :: MVar (IntMap (Lifetime (CUDA.Module, Kernel, Kernel)))
modules_f32 = unsafePerformIO $ newMVar IM.empty

{-# NOINLINE modules_f64 #-}
modules_f64 :: MVar (IntMap (Lifetime (CUDA.Module, Kernel, Kernel)))
modules_f64 = unsafePerformIO $ newMVar IM.empty

ptx_twine_f32 :: ByteString
ptx_twine_f32 = $(makeRelativeToProject "cubits/twine_f32.ptx" >>= embedFile)

ptx_twine_f64 :: ByteString
ptx_twine_f64 = $(makeRelativeToProject "cubits/twine_f64.ptx" >>= embedFile)

