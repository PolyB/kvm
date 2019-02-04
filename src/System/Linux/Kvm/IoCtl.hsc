{-# LANGUAGE ForeignFunctionInterface #-}

module System.Linux.Kvm.IoCtl where

import Foreign.C.Types
import Foreign
import Foreign.Ptr
import Foreign.C
import System.Posix
import System.Linux.Kvm.IoCtl.Exts as EXT
import System.Linux.Kvm.IoCtl.Types
import Control.Lens
import Control.Monad.Fail (fail)
import Prelude hiding (fail)
import Control.Monad (when)
import System.IO
import System.Linux.Kvm.IoCtl.Types.KvmRun
import System.Linux.Kvm.IoCtl.Types.KvmGuestDebug

#include <linux/kvm.h>
#include <sys/mman.h>

data KvmFd = KvmFd Fd
data VmFd = VmFd Fd
data VcpuFd = VcpuFd Fd

foreign import ccall "ioctl" c_ioctl :: CInt -> CInt -> Ptr () -> IO CInt
foreign import ccall "mmap" c_mmap :: Ptr () -> CInt -> CInt -> CInt -> CInt -> CInt -> IO (Ptr ())
foreign import ccall "munmap" c_munmap :: Ptr () -> CInt -> IO CInt

c_ioctl' :: Fd -> CInt -> Ptr a -> IO CInt
c_ioctl' f req p = c_ioctl (fromIntegral f) req (castPtr p)

c_ioctl'' :: Fd -> CInt -> Int -> IO CInt
c_ioctl'' f req p = c_ioctl (fromIntegral f) req (intPtrToPtr $ IntPtr $ p)

kvmFd:: IO KvmFd
kvmFd = KvmFd <$> openFd "/dev/kvm" ReadWrite Nothing defaultFileFlags


createVM :: KvmFd -> IO VmFd -- TODO : machine type identifier
createVM (KvmFd f) = do
                        res <- c_ioctl'' f (#const KVM_CREATE_VM) 0
                        when (res == -1) $ fail "kvm:ioctl:failed to create VM"
                        return $ VmFd $ Fd res

createVCPU :: VmFd -> Int -> IO VcpuFd
createVCPU (VmFd vm) i = do
                            res <- c_ioctl'' vm (#const KVM_CREATE_VCPU) i
                            when (res == -1) $ fail "kvm:ioctl:failed to create VCPU"
                            return $ VcpuFd $ Fd res 

setTssAddr :: VmFd -> Word64 -> IO ()
setTssAddr (VmFd vm) addr = do
                              res <- c_ioctl'' vm (#const KVM_SET_TSS_ADDR) (fromIntegral addr)
                              when (res == -1)  $ fail "kvm:ioctl:failed to set Tss addr"

getApiVersion :: KvmFd -> IO CInt
getApiVersion (KvmFd kvm) = c_ioctl' kvm (#const KVM_GET_API_VERSION) nullPtr

checkExtension :: KvmFd -> EXT.KVMExts -> IO CInt
checkExtension (KvmFd kvm) ext = c_ioctl'' kvm (#const KVM_CHECK_EXTENSION) (EXT.getExtCode ext)

setUserMemoryRegion :: VmFd -> UserspaceMemoryRegion -> IO ()
setUserMemoryRegion (VmFd vm) region = do
                                  res <- with region $ c_ioctl' vm (#const KVM_SET_USER_MEMORY_REGION)
                                  when (res == -1) $ fail "kvm:ioctl:failed to set user memory region"

getRegs :: VcpuFd -> IO Regs
getRegs (VcpuFd cpu) = alloca $ \ptr -> do
                                r <- c_ioctl' cpu (#const KVM_GET_REGS) ptr
                                when (r == -1) $ fail "kvm:ioctl:failed to get cpu regs"
                                peek ptr

setRegs :: VcpuFd -> Regs -> IO ()
setRegs (VcpuFd cpu) regs = do
                              res <- with regs $ c_ioctl' cpu (#const KVM_SET_REGS) 
                              when (res == -1) $ fail "kvm:ioctl:failed to set cpu regs"

getSRegs :: VcpuFd -> IO SRegs
getSRegs (VcpuFd cpu) = alloca $ \ptr -> do
                                r <- c_ioctl' cpu (#const KVM_GET_SREGS) ptr
                                when (r == -1) $ fail "kvm:ioctl:failed to get cpu sregs"
                                peek ptr

setSRegs :: VcpuFd -> SRegs -> IO ()
setSRegs (VcpuFd cpu) sregs = do
                              res <- with sregs $ c_ioctl' cpu (#const KVM_SET_SREGS) 
                              when (res == -1) $ fail "kvm:ioctl:failed to set cpu sregs"

getVcpuMmapSize :: KvmFd -> IO CInt
getVcpuMmapSize (KvmFd fd) = c_ioctl'' fd (#const KVM_GET_VCPU_MMAP_SIZE) 0

runKvm :: VcpuFd -> IO ()
runKvm (VcpuFd cpu) = do
                        r <- c_ioctl'' cpu (#const KVM_RUN) 0
                        when (r == -1) $ fail "kvm:ioctl:failed to run kvm"


setIdentityMap :: VmFd -> Word64 -> IO ()
setIdentityMap (VmFd fd) addr = do
                                  r <- with addr $ c_ioctl' fd (#const KVM_SET_IDENTITY_MAP_ADDR)
                                  when (r == -1) $ fail "kvm:ioctl:set identity map addr returned an error"

createIRQChip :: VmFd -> IO ()
createIRQChip (VmFd fd) = do
                              r <- c_ioctl'' fd (#const KVM_CREATE_IRQCHIP) 0
                              when (r == -1) $ fail "kvm:ioctl:create IRQ chip returned an error"

setGuestDebug :: VcpuFd -> KvmGuestDebug -> IO ()
setGuestDebug (VcpuFd fd) kgd = do
                                    r <- with kgd $ c_ioctl' fd (#const KVM_SET_GUEST_DEBUG)
                                    when (r == -1) $ fail "kvm:ioctl:setGuestDebug failed"


-- Mmap memory with fixed length
mmap :: Int -> IO (Ptr ())
mmap l = do
           r <- c_mmap nullPtr (fromIntegral l) ((#const PROT_READ) .|. (#const PROT_WRITE)) ((#const MAP_PRIVATE) .|. (#const MAP_ANONYMOUS)) (-1) 0
           when ((ptrToIntPtr r) == (IntPtr (-1))) $ fail "kvm:ioctl:mmap failed"
           return r


allocKvmRun :: KvmFd -> VcpuFd -> IO (Ptr KvmRun)
allocKvmRun kvm (VcpuFd fd) = do
                                 size <- getVcpuMmapSize kvm
                                 castPtr <$> c_mmap 
                                    nullPtr  -- addr
                                    size     -- length
                                    ((#const PROT_READ) .|. (#const PROT_WRITE)) -- prot
                                    (#const MAP_SHARED) --flags
                                    (fromIntegral fd) -- fd
                                    0 -- offset

freeKvmRun :: KvmFd -> Ptr KvmRun -> IO ()
freeKvmRun kvm kvmrun = do
                          size <- getVcpuMmapSize kvm
                          c_munmap (castPtr kvmrun) size
                          return ()

kvmTranslate :: VcpuFd -> Word64 -> IO Word64
kvmTranslate (VcpuFd fd) address = do
                                    allocaBytes (#size struct kvm_translation) $ \ptr -> do
                                                                                    (#poke struct kvm_translation, linear_address) ptr address
                                                                                    r <- c_ioctl' fd (#const KVM_TRANSLATE) ptr 
                                                                                    when (r == -1) $ fail "kvm:ioctl:translate returned an error"
                                                                                    (#peek struct kvm_translation, physical_address) ptr


