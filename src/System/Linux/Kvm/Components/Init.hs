{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
module System.Linux.Kvm.Components.Init (Init, InitT, MonadInit, runInit, loadBzImage) where

import qualified Ether.State as I
import qualified Ether.Except as I
import System.Linux.Kvm.KvmM.Vm
import System.Linux.Kvm.Errors
import System.Linux.Kvm.Components.Init.SetupHeader
import System.Linux.Kvm.Components.Ram
import Control.Monad.IO.Class
import System.Posix.Types
import System.Posix.IO
import Foreign.Marshal.Alloc
import Foreign.Ptr
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
bzKernelStart =  0x100000

bootLoaderSector = 0x1000
bootLoaderIp = 0


readAllFile:: (MonadIO m, MonadError m) => Fd -> Ptr () -> m ()
readAllFile fd ptr = do
                        nr <- execIO $ fdReadBuf fd (castPtr ptr) 65536
                        when (nr /= 0) $ readAllFile fd (plusPtr ptr (fromIntegral nr))

loadBzImage :: (MonadRam m, MonadIO m, MonadError m, MonadVm m, MonadInit m) => m ()
loadBzImage = (`I.evalStateT'`emptySetupHeader)$ do 
                -- get setupHeader
                kernelPath <- I.gets' kernel
                kernFd <- execIO $ openFd kernelPath ReadOnly Nothing defaultFileFlags
                setupHeader <- execIO $ readSetupHeader kernFd
                I.put' setupHeader
                -- check setupHeader
                when (header setupHeader /= setupHeaderMagic) $ I.throw' ErrorBadKernelFile
                when (version setupHeader < bootProtocolRequired) $ I.throw' ErrorKernelTooOld
                -- copy setup.bin
                let kernSetupSects = if (setup_sects setupHeader /= 0) then (setup_sects setupHeader) else bzDefaultSects
                let setupBinSize = 512 * (fromIntegral kernSetupSects)
                setupBinPos <- realToHost bootLoaderSector 0
                _ <- execIO $ fdSeek kernFd AbsoluteSeek 0
                bytesRead <- execIO $ fdReadBuf kernFd (castPtr setupBinPos) (setupBinSize)
                when (bytesRead /= setupBinSize) $ I.throw' ErrorBadKernelFile
                -- copy vmlinux.bin
                vmlinuxBinPtr <- flatToHost bzKernelStart
                readAllFile kernFd vmlinuxBinPtr
                -- copy cmdline
                -- TODO

                I.modify' (\x -> x { type_of_loader=0xff, heap_end_ptr=0xfe00, loadflags=(loadflags x .|. loadFlagsCanUseHeap)})

                -- copy initrd
                -- TODO
                I.get' >>= (\x -> execIO $ pokeSetupHeader (getSetupHeader (castPtr setupBinPos)) x)
