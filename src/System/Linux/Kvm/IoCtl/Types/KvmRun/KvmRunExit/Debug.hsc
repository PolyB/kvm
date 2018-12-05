{-# LANGUAGE TemplateHaskell #-}

module System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunExit.Debug where

import Foreign.C.Types
import Foreign
import Foreign.Ptr
import Foreign.C
import Control.Lens

#include <linux/kvm.h>

-- TODO this class is harder to implement because it is implementation independant

data Debug = Debug (Ptr ())
  deriving (Show, Eq)

peekKvmRunExitDebug :: Ptr a -> IO Debug
peekKvmRunExitDebug ptr = return $ Debug $ (#ptr struct kvm_run, debug.arch) ptr
              
