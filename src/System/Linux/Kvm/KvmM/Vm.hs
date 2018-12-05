{-# LANGUAGE TemplateHaskell #-}
module System.Linux.Kvm.KvmM.Vm where

import Control.Lens
import System.Linux.Kvm.KvmM.Kvm
import System.Linux.Kvm.IoCtl as C
import Control.Monad.State.Strict
import Foreign
import System.Linux.Kvm.IoCtl.Types.UserspaceMemoryRegion
import Data.List
import Data.Function

data VmMInternal = VmMInternal
  {
     _continueVm :: Bool
    ,_userSpaceMemRegions :: [UserspaceMemoryRegion]
    ,_vmfd :: VmFd
  }
makeLenses ''VmMInternal


type VmM = StateT VmMInternal KvmM
runVmM :: VmM a -> KvmM a
runVmM act = do
              kvm <- use kvmfd
              fd <- lift $ C.createVM kvm
              evalStateT act $ VmMInternal {
                 _continueVm = True
                ,_vmfd = fd
                ,_userSpaceMemRegions = []
             }

stopVm :: VmM ()
stopVm = continueVm .= False

setTss :: Word -> VmM ()
setTss w = do
            fd <- use vmfd
            liftIO $ C.setTssAddr fd w

setIdentityMap :: Word -> VmM ()
setIdentityMap w = do
                    fd <- use vmfd
                    liftIO $ C.setIdentityMap fd w

createIRQChip :: VmM ()
createIRQChip = do
                  fd <- use vmfd
                  liftIO $ C.createIRQChip fd

memRegions ::Getter VmMInternal [UserspaceMemoryRegion]
memRegions = userSpaceMemRegions

createMemRegion :: Int -> Word64 -> VmM (Ptr Word8)
createMemRegion size guestAddr = do
                                    ptr <- liftIO $ mmap size
                                    max_slot <- use $ userSpaceMemRegions.to (map _slot).to (\x -> if null x then 0 else 1 + (maximum x))
                                    let newslot = UserspaceMemoryRegion { _slot = max_slot, _flags = 0, _guest_phys_addr = guestAddr, _memory_size = (fromIntegral size), _userspace_addr = ptr }
                                    userSpaceMemRegions %= (newslot:)
                                    fd <- use vmfd
                                    liftIO $ setUserMemoryRegion fd newslot
                                    return $ castPtr ptr


