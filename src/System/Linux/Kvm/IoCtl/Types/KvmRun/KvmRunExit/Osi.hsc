{-# LANGUAGE TemplateHaskell #-}

module System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunExit.Osi where

import Foreign.C.Types
import Foreign
import Foreign.Ptr
import Foreign.C
import Control.Lens
import Data.Array.IArray
import System.Linux.Kvm.IoCtl.Types.Utils

#include <linux/kvm.h>

data Osi = Osi { _gprs :: Array Int Word64 }
  deriving (Show, Eq)

makeLenses ''Osi

peekKvmRunExitOsi :: Ptr a -> IO Osi
peekKvmRunExitOsi ptr = Osi <$> (mkArray 32 $ (#ptr struct kvm_run, osi.gprs) ptr)
