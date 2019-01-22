{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

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
 ,createMemRegion
 ,createMemRegionFile
 -- * Vm Properties
 ,memRegions
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
import System.Linux.Kvm.IoCtl.Types.UserspaceMemoryRegion
import System.Posix.IO
import System.Posix.Files
import Control.Monad.IO.Class
import System.Linux.Kvm.Components.Ram

data Vm = Vm
  {
     _continueVm :: Bool
    ,_userSpaceMemRegions :: [UserspaceMemoryRegion]
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
                ,_userSpaceMemRegions = []
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


mapMemRegion:: (MonadError m, MonadVm m, MonadIO m) => Ptr () -> Int -> Word64 -> m ()
mapMemRegion host size guest = do
                                    max_slot <- I.tagAttach @Vm $ use $ userSpaceMemRegions.to (map _slot).to (\x -> if null x then 0 else 1 + (maximum x))
                                    let newslot = UserspaceMemoryRegion { _slot = max_slot, _flags = 0, _guest_phys_addr = guest, _memory_size = (fromIntegral size), _userspace_addr = host }
                                    I.tagAttach @Vm $ userSpaceMemRegions %= (newslot:)
                                    fd <- vmfd
                                    execIO $ C.setUserMemoryRegion fd newslot
-- | Create a memory region initialized from a file
createMemRegionFile :: (MonadError m, MonadVm m, MonadIO m, MonadRam m) => 
                      FilePath -- ^ Path to the file
                      -> Word64  -- ^ Address target in the guest Vm
                      -> m (Ptr Word8) -- ^ A 'Ptr' to where the memory region is mapped in the file
createMemRegionFile path guestAddr = let align i v = case v `mod` i of
                                                       0 -> v
                                                       n -> v + (i - n)
                                     in do
                                        fd <- execIO $ openFd path ReadOnly Nothing defaultFileFlags
                                        size <- liftIO $ align 4096<$> fromIntegral<$> fileSize <$> getFileStatus path
                                        ptr <- castPtr <$> mmap size
                                        liftIO $ fdReadBuf fd ptr (fromIntegral size)
                                        mapMemRegion (castPtr ptr) size guestAddr
                                        return $ ptr

-- | Create an uninitialized memory region
createMemRegion :: (MonadError m, MonadVm m, MonadIO m, MonadRam m) => 
                   Int -- ^ The size of the memory region
                   -> Word64 -- ^ Address target in the guest Vm
                   -> m (Ptr Word8) -- ^ A 'Ptr' to where the memory region is mapped in the file
createMemRegion size guestAddr = do
                                    ptr <- mmap size
                                    mapMemRegion ptr size guestAddr
                                    return $ castPtr ptr

-- | 'Getter' to the 'userSpaceMemRegions' list of the Vm
memRegions ::Getter Vm [UserspaceMemoryRegion]
memRegions = userSpaceMemRegions

vmfd :: (MonadVm m) => m C.VmFd
vmfd = I.gets' _fd

doContinueVm :: (MonadVm m) => m Bool
doContinueVm = I.gets' _continueVm
