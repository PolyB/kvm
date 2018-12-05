{-# LANGUAGE TemplateHaskell #-}

module System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunExit.IoapicEoi where

import Foreign.C.Types
import Foreign
import Foreign.Ptr
import Foreign.C
import Control.Lens
import Data.Array.IArray
import System.Linux.Kvm.IoCtl.Types.Utils

#include <linux/kvm.h>

data IoapicEoi = IoapicEoi { _vector :: Word8 }
  deriving (Show, Eq)

makeLenses ''IoapicEoi

peekKvmRunExitIoapicEoi :: Ptr a -> IO IoapicEoi
peekKvmRunExitIoapicEoi ptr = IoapicEoi <$> (#peek struct kvm_run, eoi.vector) ptr
