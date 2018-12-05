{-# LANGUAGE TemplateHaskell #-}

module System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunExit.Mmio where

import Foreign.C.Types
import Foreign
import Foreign.Ptr
import Foreign.C
import Control.Lens
import Data.Array.IArray
import System.Linux.Kvm.IoCtl.Types.Utils

#include <linux/kvm.h>

data Mmio = Mmio { _phys_addr :: Word64
                  ,_mdata :: Array Int Word8
                  ,_len :: Word32
                  ,_is_write :: Bool
                  }
  deriving (Show, Eq)

makeLenses ''Mmio


mkBool :: Word8 -> Bool
mkBool 0 = False
mkBool _ = True

peekKvmRunExitMmio :: Ptr a -> IO Mmio
peekKvmRunExitMmio ptr = Mmio <$> (#peek struct kvm_run, mmio.phys_addr) ptr
                          <*> (mkArray 8 $ (#ptr struct kvm_run, mmio.data) ptr)
                          <*> (#peek struct kvm_run, mmio.len) ptr
                          <*> ((#peek struct kvm_run, mmio.is_write) ptr >>= return.mkBool)
              
