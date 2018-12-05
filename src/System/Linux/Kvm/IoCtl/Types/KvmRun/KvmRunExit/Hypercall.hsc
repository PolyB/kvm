{-# LANGUAGE TemplateHaskell #-}

module System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunExit.Hypercall where

import Foreign.C.Types
import Foreign
import Foreign.Ptr
import Foreign.C
import Control.Lens
import Data.Array.IArray
import System.Linux.Kvm.IoCtl.Types.Utils

#include <linux/kvm.h>

data Hypercall = Hypercall { _nr :: Word64
                            ,_args :: Array Int Word64
                            ,_ret :: Word64
                            ,_longmode :: Word32
                            ,_pad :: Word32
                           }
  deriving (Show, Eq)

makeLenses ''Hypercall

peekKvmRunExitHypercall :: Ptr a -> IO Hypercall
peekKvmRunExitHypercall ptr = Hypercall <$> (#peek struct kvm_run, hypercall.nr) ptr
                                    <*> (mkArray 6 $ (#ptr struct kvm_run, hypercall.args) ptr)
                                    <*> (#peek struct kvm_run, hypercall.ret) ptr
                                    <*> (#peek struct kvm_run, hypercall.longmode) ptr
                                    <*> (#peek struct kvm_run, hypercall.pad) ptr
              
