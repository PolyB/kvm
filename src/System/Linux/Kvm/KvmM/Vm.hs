{-# LANGUAGE TemplateHaskell #-}
module System.Linux.Kvm.KvmM.Vm
(
  -- * Vm Monad
  VmM
 ,runVmM
 -- * Vm Actions
 ,stopVm
 ,setTss
 ,setIdentityMap
 ,createIRQChip
 ,createMemRegion
 ,createMemRegionFile
 -- * Vm Properties
 ,memRegions
 ,continueVm
 ,vmfd
 
)
where

import Control.Lens
import System.Linux.Kvm.KvmM.Kvm
import qualified System.Linux.Kvm.IoCtl as C
import Control.Monad.State.Strict
import Foreign
import System.Linux.Kvm.IoCtl.Types.UserspaceMemoryRegion
import Data.List
import Data.Function
import System.Posix.IO
import System.Posix.Files

data VmMInternal = VmMInternal
  {
     _continueVm :: Bool
    ,_userSpaceMemRegions :: [UserspaceMemoryRegion]
    ,_vmfd :: C.VmFd
  }
makeLenses ''VmMInternal

-- | The 'VmM' monad transformer, 
-- 'VmM' is a 'MonadTrans' of 'Kvm'
type VmM m = StateT VmMInternal (KvmM m)

-- | Create a Vm and run it inside a 'KvmM' m monad
runVmM :: MonadIO m => VmM m a -> KvmM m a
runVmM act = do
              kvm <- use kvmfd
              fd <- liftIO $ C.createVM kvm
              evalStateT act $ VmMInternal {
                 _continueVm = True
                ,_vmfd = fd
                ,_userSpaceMemRegions = []
             }

stopVm :: Monad m => VmM m ()
stopVm = continueVm .= False

-- | Calls the /KVM_SET_TSS_ADDR/ ioctl with the given word
setTss :: MonadIO m => Word64 -> VmM m ()
setTss w = do
            fd <- use vmfd
            liftIO $ C.setTssAddr fd w

-- | calls /KVM_SET_IDENTITY_MAP_ADDR/ ioctl with the given 'Word64'
setIdentityMap :: MonadIO m => Word64 -> VmM m ()
setIdentityMap w = do
                    fd <- use vmfd
                    liftIO $ C.setIdentityMap fd w

-- | Calls the /KVM_CREATE_IRQCHIP/ ioctl
createIRQChip :: MonadIO m => VmM m ()
createIRQChip = do
                  fd <- use vmfd
                  liftIO $ C.createIRQChip fd


mapMemRegion:: MonadIO m => Ptr () -> Int -> Word64 -> VmM m ()
mapMemRegion host size guest = do
                                    max_slot <- use $ userSpaceMemRegions.to (map _slot).to (\x -> if null x then 0 else 1 + (maximum x))
                                    let newslot = UserspaceMemoryRegion { _slot = max_slot, _flags = 0, _guest_phys_addr = guest, _memory_size = (fromIntegral size), _userspace_addr = host }
                                    userSpaceMemRegions %= (newslot:)
                                    fd <- use vmfd
                                    liftIO $ C.setUserMemoryRegion fd newslot
-- | Create a memory region initialized from a file
createMemRegionFile :: MonadIO m => 
                      FilePath -- ^ Path to the file
                      -> Word64  -- ^ Address target in the guest Vm
                      -> VmM m (Ptr Word8) -- ^ A 'Ptr' to where the memory region is mapped in the file
createMemRegionFile path guestAddr = let align i v = case v `mod` i of
                                                       0 -> v
                                                       n -> v + (i - n)
                                     in do
                                        fd <- liftIO $ openFd path ReadOnly Nothing defaultFileFlags
                                        size <- liftIO $ align 4096<$> fromIntegral<$> fileSize <$> getFileStatus path
                                        ptr <- liftIO $ castPtr <$> C.mmap size
                                        liftIO $ fdReadBuf fd ptr (fromIntegral size)
                                        mapMemRegion (castPtr ptr) size guestAddr
                                        return $ ptr

-- | Create an uninitialized memory region
createMemRegion :: MonadIO m => 
                   Int -- ^ The size of the memory region
                   -> Word64 -- ^ Address target in the guest Vm
                   -> VmM m (Ptr Word8) -- ^ A 'Ptr' to where the memory region is mapped in the file
createMemRegion size guestAddr = do
                                    ptr <- liftIO $ C.mmap size
                                    mapMemRegion ptr size guestAddr
                                    return $ castPtr ptr

-- | 'Getter' to the 'userSpaceMemRegions' list of the Vm
memRegions ::Getter VmMInternal [UserspaceMemoryRegion]
memRegions = userSpaceMemRegions
