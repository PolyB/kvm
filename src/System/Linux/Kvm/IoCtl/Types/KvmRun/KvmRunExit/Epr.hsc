{-# LANGUAGE TemplateHaskell #-}

module System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunExit.Epr where

import Foreign.C.Types
import Foreign
import Foreign.Ptr
import Foreign.C
import Control.Lens
import Data.Array.IArray
import System.Linux.Kvm.IoCtl.Types.Utils

#include <linux/kvm.h>

data Epr = Epr { _epr :: Word32 }
  deriving (Show, Eq)

makeLenses ''Epr

peekKvmRunExitEpr :: Ptr a -> IO Epr
peekKvmRunExitEpr ptr = Epr <$> (#peek struct kvm_run, epr.epr) ptr
