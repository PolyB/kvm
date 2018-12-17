{-# LANGUAGE TemplateHaskell #-}
module System.Linux.Kvm.KvmM.Kvm
(
  -- * Vm Monad
  KvmM
 ,runKvmM
  -- * Vm Properties
 ,kvmfd
)
where

import Control.Lens
import System.Linux.Kvm.IoCtl
import Control.Monad.State.Strict
import Control.Monad.IO.Class
import System.Posix.IO

data KvmMInternal = KvmMInternal
  {
    _kvmfd :: KvmFd
  }
makeLenses ''KvmMInternal

-- | A 'Monad' where Kvm runs in
type KvmM m = StateT KvmMInternal m

-- | Runs the 'KvmM' monad inside the 'm' monad
runKvmM::MonadIO m => KvmM m a -> m a
runKvmM act = do
                kvmfd@(KvmFd fd) <- liftIO kvmFd
                res <- evalStateT act $ KvmMInternal {
                    _kvmfd = kvmfd
                }
                liftIO $ closeFd fd
                return res
