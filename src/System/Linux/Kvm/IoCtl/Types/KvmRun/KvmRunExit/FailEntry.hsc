{-# LANGUAGE TemplateHaskell #-}

module System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunExit.FailEntry where

import Foreign.C.Types
import Foreign
import Foreign.Ptr
import Foreign.C
import Control.Lens

#include <linux/kvm.h>

data FailEntry = FailEntry { _hardware_entry_failure_reason :: Word64 }
  deriving (Show, Eq)

makeLenses ''FailEntry

peekKvmRunExitFailEntry :: Ptr a -> IO FailEntry
peekKvmRunExitFailEntry ptr = FailEntry <$> (#peek struct kvm_run, fail_entry.hardware_entry_failure_reason) ptr
