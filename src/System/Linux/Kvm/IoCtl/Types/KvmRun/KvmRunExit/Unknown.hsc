{-# LANGUAGE TemplateHaskell #-}

module System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunExit.Unknown where

import Foreign.C.Types
import Foreign
import Foreign.Ptr
import Foreign.C
import Control.Lens

#include <linux/kvm.h>

data Unknown = Unknown { _hardware_exit_reason :: Word64 }
  deriving (Show, Eq)

makeLenses ''Unknown

peekKvmRunExitUnknown :: Ptr a -> IO Unknown
peekKvmRunExitUnknown ptr = Unknown <$> (#peek struct kvm_run, hw.hardware_exit_reason) ptr
