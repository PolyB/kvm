{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.Linux.Kvm.Components.Init (Init, InitT, MonadInit, runInit, loadBzImage, initCpuRegs) where

import System.Linux.Kvm.Components.Init.E820Entry
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Word
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import GHC.IO.Device
import System.Linux.Kvm.Components.Init.SetupHeader
import System.Linux.Kvm.Components.Ram
import System.Linux.Kvm.Errors
import qualified System.Linux.Kvm.IoCtl as C
import System.Linux.Kvm.IoCtl.Types.Regs
import System.Linux.Kvm.IoCtl.Types.SRegs
import System.Linux.Kvm.IoCtl.Types.Segment
import System.Linux.Kvm.KvmM.Cpu
import System.Linux.Kvm.KvmM.Vm
import System.Posix.IO
import System.Posix.Files
import System.Posix.Types
import qualified Ether.Except as I
import qualified Ether.Reader as I
import qualified Ether.State as I
import qualified Ether.TagDispatch as I
import System.Linux.Kvm.Components.Init.BootParams
import Data.Bits

data Init = Init
    { 
        kernel :: String
        ,initrd :: Maybe String
        ,cmdline :: String
    }

type InitT m = I.StateT' Init m

type MonadInit m = I.MonadState' Init m

runInit:: Monad m => String -> Maybe String -> String -> I.StateT' Init m a -> m a
runInit ker ini cmd m = do
                            I.evalStateT' m $ Init { kernel=ker, initrd=ini, cmdline=cmd}


bzKernelStart :: Word64
bzKernelStart = 0x100000

bootParamsStart :: Word64
bootParamsStart = 0x20000

kernelCmdLinePos :: Word64
kernelCmdLinePos = 0x40000

readAllFile:: (MonadIO m, MonadError m) => Fd -> Ptr () -> Int -> m ()
readAllFile fd ptr count = do
                        when (count < 0) $ throwE ErrorTooMuchRamUsed "readAllFile"
                        nr <- execIO $ fromIntegral <$> fdReadBuf fd (castPtr ptr) (100 * 65536)
                        when (nr /= 0) $ readAllFile fd (plusPtr ptr nr) (count - nr)

copyBootParams:: (Monad m, MonadRam m, MonadIO m, MonadError m) => Fd -> m (Ptr BootParams)
copyBootParams kernFd = do
                                bootParamsPtr <- castPtr <$> flatToHost' bootParamsStart
                                let setupHeaderPtr = getSetupHeader bootParamsPtr
                                _ <- execIO $ fdSeek kernFd AbsoluteSeek (fromIntegral bootParamsOffset)
                                (execIO $ fdReadBuf kernFd (castPtr setupHeaderPtr) (fromIntegral setupHeaderSize)) >>= (\nbytes -> when (nbytes /= (fromIntegral setupHeaderSize)) $ throwE ErrorBadKernelFile "copyBootParams")
                                return bootParamsPtr


copyVmLinux :: (MonadBootParams m, MonadIO m, MonadConfigRam m, MonadError m, MonadRam m) => Fd -> Word64 -> Int -> m ()
copyVmLinux fd pos seek = exTag "copyVmLinux" $ do
                            _ <- execIO $ fdSeek fd AbsoluteSeek (fromIntegral seek)
                            vmlinuxPtr <- flatToHost' pos
                            ramMax <- I.asks' maxRam
                            readAllFile fd vmlinuxPtr (ramMax - (fromIntegral pos))

writeCmdLine :: (MonadIO m, MonadInit m, MonadRam m, MonadBootParams m, MonadError m) => Word64 -> m ()
writeCmdLine pos = do
                    cmdlineV <- I.gets' cmdline
                    cmdlinePtr <- flatToHost' pos
                    liftIO $ withCAStringLen cmdlineV $ \(cstr, len) -> copyBytes (castPtr cmdlinePtr) cstr len
                    setCmdLinePtr $ fromIntegral pos

setupInitRd :: (MonadIO m, MonadInit m, MonadRam m, MonadBootParams m, MonadError m, MonadVm m) => String -> m ()
setupInitRd initrdPath = exTag "setupInitRd" $ do
                            initrdFd <- execIO $ openFd initrdPath ReadOnly Nothing defaultFileFlags
                            size <- execIO $ fileSize <$> getFdStatus initrdFd
                            addr_max <- getInitrdAddrMax
                            -- TODO : check sizes
                            let addr_start = addr_max - (fromIntegral size)
                            let addr_start_aligned = addr_start - (addr_start `mod`0x200000)
                            let size_aligned = size + (0x200000 - size `mod`0x200000)
                            liftIO $ print size_aligned
                            initrdptr <- execIO $ C.mmapFd initrdFd (fromIntegral size)
                            mapMemRegion initrdptr (fromIntegral size_aligned) (fromIntegral addr_start_aligned)

                            setRamDiskImage addr_start_aligned
                            setRamDiskSize (fromIntegral size)
                    
                        
withM  :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
withM (Just a) m = m a
withM Nothing _ = return ()

loadBzImage :: (MonadRam m, MonadConfigRam m, MonadIO m, MonadError m, MonadVm m, MonadInit m) => m ()
loadBzImage = do
                kernelPath <- I.gets' kernel
                kernFd <- execIO $ openFd kernelPath ReadOnly Nothing defaultFileFlags

                bootParams <- copyBootParams kernFd
                
                withBootParams bootParams $ do 
                                                checkHeader
                                                getBootProtocol >>= \protocol -> when (protocol < 0x206) $ throwE ErrorKernelTooOld "loadBzImage"

                                                setupBinSize <- getSetupBinSize
                                                copyVmLinux kernFd bzKernelStart setupBinSize

                                                writeCmdLine kernelCmdLinePos

                                                setLoaderType 0xff
                                                setHeapEndPtr 0xfe00
                                                setLoadFlags (loadFlagsCanUseHeap <> loadFlagsKeepSegments <> loadFlagsLoadedHigh)

                                                ramMax <- I.asks' maxRam
                                                writeE820Entries [E820Entry { addr=0, size=0x100000, entrytype=E820_Ram }, E820Entry { addr=0x100000, size=(fromIntegral ramMax - 0x100000), entrytype=E820_Ram }]


                                                initrdCfg <- I.gets' initrd
                                                withM initrdCfg setupInitRd                 





setupRegs:: Regs -> Regs
setupRegs x = x { _rflags = 0x2
                , _rip = bzKernelStart
                , _rsp = 0
                , _rbp = 0
                , _rdi = 0
                , _rbx = 0
                , _rsi = bootParamsStart
                }


setupSRegs:: SRegs -> SRegs
setupSRegs x = x { _cs = (x^.cs) { _base = 0, _limit = 0xffffffff, _g = 1, _present = 1, _db = 1, _selector = 0x10, _stype = segToWord segExecuteRead }
                 , _ss = (x^.ss) { _base = 0, _limit = 0xffffffff, _g = 1, _present = 1, _db = 1, _selector = 0x18, _stype = segToWord segReadWrite }
                 , _ds = (x^.ds) { _base = 0, _limit = 0xffffffff, _g = 1, _present = 1, _selector = 0x18, _stype = segToWord segReadWrite }
                 , _es = (x^.es) { _base = 0, _limit = 0xffffffff, _g = 1, _present = 1, _selector = 0x18, _stype = segToWord segReadWrite }
                 , _fs = (x^.fs) { _base = 0, _limit = 0xffffffff, _g = 1, _present = 1 }
                 , _gs = (x^.gs) { _base = 0, _limit = 0xffffffff, _g = 1, _present = 1 }
                 , _cr0 = setBit (x^.cr0) 0
                }
initCpuRegs::(MonadIO m, MonadCpu m) => m ()
initCpuRegs = lmodify setupRegs >> lmodify setupSRegs
