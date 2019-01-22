{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module System.Linux.Kvm.KvmM.Kvm
(
  -- * KVM Monad Transformer
  KvmT
 ,runKvmT
 ,MonadKvm
  -- * KVM Properties
 ,kvmfd
)
where

import qualified System.Linux.Kvm.IoCtl as C
import System.Linux.Kvm.Errors
import System.Posix.IO
import Control.Monad.IO.Class
import qualified Ether.Reader as I

data Kvm = Kvm
  {
    _kvmfd :: C.KvmFd
  }

-- | A 'MonadTransformer' where Kvm runs in
type KvmT m = I.ReaderT' Kvm m

type MonadKvm m = I.MonadReader' Kvm m

-- | Runs the 'KvmT' 'monadTransformer'
runKvmT::(MonadError m, MonadIO m) => KvmT m a -> m a
runKvmT act = do
                kvmfd@(C.KvmFd fd) <- execIO C.kvmFd
                res <- I.runReaderT' act $ Kvm {
                    _kvmfd = kvmfd
                }
                execIO $ closeFd fd
                return res

kvmfd :: MonadKvm m => m C.KvmFd
kvmfd = I.asks' _kvmfd
