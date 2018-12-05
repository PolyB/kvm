{-# LANGUAGE TemplateHaskell #-}
module System.Linux.Kvm.KvmM.Kvm where

import Control.Lens
import System.Linux.Kvm.IoCtl
import Control.Monad.State.Strict

data KvmMInternal = KvmMInternal
  {
    _kvmfd :: KvmFd
  }
makeLenses ''KvmMInternal

type KvmM = StateT KvmMInternal IO

runKvmM::KvmM a -> IO a
runKvmM act = do
                fd <- kvmFd
                evalStateT act $ KvmMInternal {
                    _kvmfd = fd
                }
                -- TODO : close Fd
