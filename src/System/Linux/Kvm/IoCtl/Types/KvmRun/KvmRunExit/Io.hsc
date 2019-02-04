{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunExit.Io where

import Foreign.C.Types
import Foreign
import Foreign.Ptr
import Foreign.C
import Control.Lens
import System.Linux.Kvm.IoCtl.Types.Utils
import Data.Array.IArray

#include <linux/kvm.h>

data IoDirection = IoDirectionIn (Ptr Word8)
                 | IoDirectionOut (Array Int Word8)
                 | IoDirectionUnknown
  deriving (Show, Eq)


data Io = Io { _direction :: IoDirection, _port :: Word16, _count :: Word32 }
  deriving (Show, Eq)

makeLenses ''Io

mkDirection :: Word8 -> Ptr a -> IO IoDirection
mkDirection (#const KVM_EXIT_IO_IN) ptr = IoDirectionIn <$> mkDataPtr ptr
mkDirection (#const KVM_EXIT_IO_OUT) ptr = IoDirectionOut <$> mkData ptr
mkDirection _ _ = return IoDirectionUnknown

mkDataPtr :: Ptr a -> IO (Ptr Word8)
mkDataPtr ptr = do
                    (offset :: Word64) <- (#peek struct kvm_run, io.data_offset) ptr
                    return $ plusPtr ptr (fromIntegral offset)

mkData :: Ptr a -> IO (Array Int Word8)
mkData ptr = do
              (size :: Word8) <- (#peek struct kvm_run, io.size) ptr
              dataptr <- mkDataPtr ptr
              mkArray (fromIntegral size) dataptr


peekKvmRunExitIo :: Ptr a -> IO Io
peekKvmRunExitIo ptr = do dir <- (#peek struct kvm_run, io.direction) ptr
                          Io <$> mkDirection dir ptr
                             <*> (#peek struct kvm_run, io.port) ptr
                             <*> (#peek struct kvm_run, io.count) ptr
