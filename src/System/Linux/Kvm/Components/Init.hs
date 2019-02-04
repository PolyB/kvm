{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module System.Linux.Kvm.Components.Init (Init, InitT, MonadInit, runInit, loadBzImage, initCpuRegs) where

import qualified Ether.State as I
import qualified Ether.Except as I
import qualified Ether.TagDispatch as I
import System.Linux.Kvm.KvmM.Vm
import System.Linux.Kvm.KvmM.Cpu
import System.Linux.Kvm.Errors
import System.Linux.Kvm.IoCtl.Types.Segment
import System.Linux.Kvm.Components.Init.SetupHeader
import System.Linux.Kvm.Debug
import System.Linux.Kvm.IoCtl.Types.Regs
import System.Linux.Kvm.IoCtl.Types.SRegs
import System.Linux.Kvm.Components.Ram
import Control.Monad.IO.Class
import Control.Lens
import System.Posix.Types
import System.Posix.IO
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.C.String
import Control.Monad
import GHC.IO.Device
import Data.Word
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


readSetupHeader :: Fd -> IO SetupHeader
readSetupHeader kernFd = allocaBytes bootParamsSize $ \bootParamsPtr -> do
                                                    _ <- fdSeek kernFd AbsoluteSeek 0
                                                    bytesRead <- fdReadBuf kernFd (castPtr bootParamsPtr) (fromIntegral bootParamsSize)
                                                    when (bytesRead /= (fromIntegral bootParamsSize)) $ fail "bad kernel file"
                                                    peekSetupHeader (getSetupHeader bootParamsPtr)


bootProtocolRequired :: Word16
bootProtocolRequired = 0x206

bzDefaultSects = 4
bzKernelStart =  0x100000 :: Word64


bootParamsStart :: Word64
bootParamsStart = bzKernelStart - fromIntegral setupHeaderSize

readAllFile:: (MonadIO m, MonadError m) => Fd -> Ptr () -> m ()
readAllFile fd ptr = do
                        nr <- execIO $ fdReadBuf fd (castPtr ptr) 65536
                        when (nr /= 0) $ readAllFile fd (plusPtr ptr (fromIntegral nr))

loadBzImage :: (MonadRam m, MonadIO m, MonadError m, MonadVm m, MonadInit m) => m ()
loadBzImage = do
                kernelPath <- I.gets' kernel
                kernFd <- execIO $ openFd kernelPath ReadOnly Nothing defaultFileFlags
                setupHeader <- execIO $ readSetupHeader kernFd
                setupHeader <- (`I.execStateT'`setupHeader)$ do 
                    -- check setupHeader
                    when (header setupHeader /= setupHeaderMagic) $ I.throw' ErrorBadKernelFile
                    when (version setupHeader < bootProtocolRequired) $ I.throw' ErrorKernelTooOld
                    -- copy setup.bin
                    let kernSetupSects = if (setup_sects setupHeader /= 0) then (setup_sects setupHeader) else bzDefaultSects
                    let setupBinSize = 512 * (1 + fromIntegral kernSetupSects)
                    -- _ <- execIO $ fdSeek kernFd AbsoluteSeek 0
                    -- bytesRead <- execIO $ fdReadBuf kernFd (castPtr setupBinPos) (setupBinSize)
                    -- when (bytesRead /= setupBinSize) $ I.throw' ErrorBadKernelFile

                    -- copy vmlinux.bin
                    _ <- execIO $ fdSeek kernFd AbsoluteSeek (fromIntegral setupBinSize)
                    vmlinuxBinPtr <- flatToHost bzKernelStart
                    readAllFile kernFd vmlinuxBinPtr
                    -- copy cmdline
                    cmdlineV <- I.gets' cmdline
                    let cmdlineAddr = bootParamsStart - fromIntegral (length cmdlineV + 1)
                    cmdlinePtr <- flatToHost cmdlineAddr
                    liftIO $ withCAStringLen cmdlineV $ \(cstr, len) -> copyBytes (castPtr cmdlinePtr) cstr len

                    I.modify' (\x -> x { type_of_loader=0xff, heap_end_ptr=0xfe00, loadflags=(loadflags x .|. loadFlagsCanUseHeap .|. loadFlagsKeepSegments), cmd_line_ptr=(fromIntegral cmdlineAddr)})

                    -- copy initrd
                    -- TODO
                bootParamsPtr <- flatToHost bootParamsStart
                execIO $ pokeSetupHeader (getSetupHeader (castPtr bootParamsPtr)) setupHeader

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
setupSRegs x = let selToBase s = 16 * (fromIntegral s)
                   segment seg = seg { _base = 0, _limit = 0xffffffff, _g = 1, _present = 1 }
                in x { _cs = segment (x^.cs) { _db = 1, _selector = 0x10, _stype = segToWord segExecuteRead }
                     , _ss = segment (x^.ss) { _db = 1, _selector = 0x18, _stype = segToWord segReadWrite }
                     , _ds = segment (x^.ds) { _selector = 0x18, _stype = segToWord segReadWrite }
                     , _es = segment (x^.es) { _selector = 0x18, _stype = segToWord segReadWrite }
                     , _fs = segment (x^.fs)
                     , _gs = segment (x^.gs)
                     , _cr0 = setBit (x^.cr0) 0
                     }
initCpuRegs::MonadCpu m => m ()
initCpuRegs = I.tagAttach @Cpu $ do
                                    regs %= setupRegs
                                    sregs %= setupSRegs
