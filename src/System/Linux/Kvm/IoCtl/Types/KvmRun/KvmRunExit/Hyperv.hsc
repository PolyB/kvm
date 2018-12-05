{-# LANGUAGE TemplateHaskell #-}

module System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunExit.Hyperv where

import Foreign.C.Types
import Foreign
import Foreign.Ptr
import Foreign.C
import Control.Lens
import Data.Array.IArray
import System.Linux.Kvm.IoCtl.Types.Utils

#include <linux/kvm.h>

-- TODO

data Hyperv = Hyperv (Ptr ())
  deriving (Show, Eq)

peekKvmRunExitHyperv :: Ptr a -> IO Hyperv
peekKvmRunExitHyperv ptr = return $ Hyperv $ (#ptr struct kvm_run, hyperv) ptr
