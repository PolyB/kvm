{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module System.Linux.Kvm.KvmM.Vm
(
  -- * Vm Monad
  VmT
 ,runVmT
 ,MonadVm
 -- * Vm Actions
 ,stopVm
 ,setTss
 ,setIdentityMap
 ,createIRQChip
 ,createPit2
 ,irqLine
 -- * Vm Properties
 ,doContinueVm
 ,vmfd
 
)
where

import Control.Lens
import System.Linux.Kvm.KvmM.Kvm
import qualified System.Linux.Kvm.IoCtl as C
import qualified Ether.State as I
import qualified Ether.TagDispatch as I
import Foreign
import System.Linux.Kvm.Errors
import System.Posix.IO
import Control.Monad.IO.Class
import System.Linux.Kvm.IoCtl.Types.PitConfig
-- import System.Linux.Kvm.Components.Ram

data Vm = Vm
  {
     _continueVm :: Bool
    ,_fd :: C.VmFd
  }
makeLenses ''Vm

-- | The 'VmM' monad transformer, 
-- 'VmM' is a 'MonadTrans' of 'Kvm'
type VmT m = I.StateT' Vm m

type MonadVm m = I.MonadState' Vm m

-- | Create a Vm and run it inside a 'KvmM' m monad
runVmT :: (MonadError m, MonadKvm m, MonadIO m) => VmT m a -> m a
runVmT act = do
              kvm <- kvmfd
              fd@(C.VmFd rawfd) <- execIO $ C.createVM kvm
              res <- I.evalStateT' act $ Vm {
                 _continueVm = True
                ,_fd = fd
              }
              execIO $ closeFd rawfd
              return res

stopVm :: (MonadVm m) => m ()
stopVm = I.tagAttach @Vm $ continueVm .= False

-- | Calls the /KVM_SET_TSS_ADDR/ ioctl with the given word
setTss :: (MonadError m, MonadVm m, MonadIO m) => Word64 -> m ()
setTss w = do
            fd <- vmfd
            execIO $ C.setTssAddr fd w

-- | calls /KVM_SET_IDENTITY_MAP_ADDR/ ioctl with the given 'Word64'
setIdentityMap :: (MonadError m, MonadVm m, MonadIO m) => Word64 -> m ()
setIdentityMap w = do
                    fd <- vmfd
                    execIO $ C.setIdentityMap fd w

-- | Calls the /KVM_CREATE_IRQCHIP/ ioctl
createIRQChip :: (MonadError m, MonadVm m, MonadIO m) => m ()
createIRQChip = do
                  fd <- vmfd
                  execIO $ C.createIRQChip fd

vmfd :: (MonadVm m) => m C.VmFd
vmfd = I.gets' _fd

doContinueVm :: (MonadVm m) => m Bool
doContinueVm = I.gets' _continueVm

createPit2 :: (MonadVm m, MonadIO m, MonadError m) => PitConfig -> m ()
createPit2 pit = do
                  fd <- vmfd
                  execIO $ C.kvmCreatePit2 fd pit

irqLine :: (MonadIO m, MonadVm m) => Word32 -> Word32 -> m ()
irqLine irq level = do
                        fd <- vmfd
                        liftIO $ C.kvmIrqLine fd irq level
