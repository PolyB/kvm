{-# LANGUAGE TemplateHaskell #-}

module System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunExit.InternalError where

import Foreign.C.Types
import Foreign
import Foreign.Ptr
import Foreign.C
import Control.Lens
import Data.Array.IArray
import System.Linux.Kvm.IoCtl.Types.Utils

#include <linux/kvm.h>

data InternalError = InternalError { _suberror :: Word32
                                    ,_ndata :: Word32
                                    ,_errdata :: Array Int Word64
                                   }
  deriving (Show, Eq)

makeLenses ''InternalError

peekKvmRunExitInternalError :: Ptr a -> IO InternalError
peekKvmRunExitInternalError ptr = InternalError <$> (#peek struct kvm_run, internal.suberror) ptr
                                            <*> (#peek struct kvm_run, internal.ndata) ptr
                                            <*> (mkArray 16 $ (#ptr struct kvm_run, internal.data) ptr)
