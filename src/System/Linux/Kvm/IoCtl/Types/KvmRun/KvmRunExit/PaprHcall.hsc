{-# LANGUAGE TemplateHaskell #-}

module System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunExit.PaprHcall where

import Foreign.C.Types
import Foreign
import Foreign.Ptr
import Foreign.C
import Control.Lens
import Data.Array.IArray
import System.Linux.Kvm.IoCtl.Types.Utils

#include <linux/kvm.h>

data PaprHcall = PaprHcall { _nr :: Word64
                            ,_ret :: Word64
                            ,_args :: Array Int Word64
                           }
  deriving (Show, Eq)

makeLenses ''PaprHcall

peekKvmRunExitPaprHcall :: Ptr a -> IO PaprHcall
peekKvmRunExitPaprHcall ptr = PaprHcall <$> (#peek struct kvm_run, papr_hcall.nr) ptr
                                            <*> (#peek struct kvm_run, papr_hcall.ret) ptr
                                            <*> (mkArray 9 $ (#ptr struct kvm_run, papr_hcall.args) ptr)
