{-# LANGUAGE TemplateHaskell #-}

module System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunExit.Exception where

import Foreign.C.Types
import Foreign
import Foreign.Ptr
import Foreign.C
import Control.Lens

#include <linux/kvm.h>

data Exception = Exception { _exception :: Word32, _error_code :: Word32 }
  deriving (Show, Eq)

makeLenses ''Exception

peekKvmRunExitException :: Ptr a -> IO Exception
peekKvmRunExitException ptr = Exception <$> (#peek struct kvm_run, ex.exception) ptr
                                        <*> (#peek struct kvm_run, ex.error_code) ptr
              
