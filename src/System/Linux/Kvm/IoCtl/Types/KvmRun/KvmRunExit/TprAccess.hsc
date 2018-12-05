{-# LANGUAGE TemplateHaskell #-}

module System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunExit.TprAccess where

import Foreign.C.Types
import Foreign
import Foreign.Ptr
import Foreign.C
import Control.Lens
import Data.Array.IArray
import System.Linux.Kvm.IoCtl.Types.Utils

#include <linux/kvm.h>

data TprAccess = TprAccess { _rip :: Word64
                            ,_is_write :: Word32
                            ,_pad :: Word32
                           }
  deriving (Show, Eq)

makeLenses ''TprAccess

peekKvmRunExitTprAccess :: Ptr a -> IO TprAccess
peekKvmRunExitTprAccess ptr = TprAccess <$> (#peek struct kvm_run, tpr_access.rip) ptr
                                    <*> (#peek struct kvm_run, tpr_access.is_write) ptr
                                    <*> (#peek struct kvm_run, tpr_access.pad) ptr
