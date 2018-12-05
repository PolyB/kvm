{-# LANGUAGE TemplateHaskell #-}

module System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunExit.SystemEvent where

import Foreign.C.Types
import Foreign
import Foreign.Ptr
import Foreign.C
import Control.Lens
import Data.Array.IArray
import System.Linux.Kvm.IoCtl.Types.Utils

#include <linux/kvm.h>

data SystemEvent = SystemEvent { _evtype :: Word32, _flags :: Word64 }
  deriving (Show, Eq)

makeLenses ''SystemEvent

peekKvmRunExitSystemEvent :: Ptr a -> IO SystemEvent
peekKvmRunExitSystemEvent ptr = SystemEvent <$> (#peek struct kvm_run, system_event.type) ptr
                                        <*> (#peek struct kvm_run, system_event.flags) ptr
