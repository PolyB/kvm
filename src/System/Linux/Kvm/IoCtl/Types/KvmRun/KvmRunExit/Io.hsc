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

data IoDirection = IoDirectionIn
                 | IoDirectionOut
                 | IoDirectionUnknown
  deriving (Show, Eq)


data Io = Io { _direction :: IoDirection, _port :: Word16, _count :: Word32, _iodata :: Array Int Word8 }
  deriving (Show, Eq)

makeLenses ''Io

mkDirection :: Word8 -> IoDirection
mkDirection (#const KVM_EXIT_IO_IN) = IoDirectionIn
mkDirection (#const KVM_EXIT_IO_OUT) = IoDirectionOut
mkDirection _ = IoDirectionUnknown

mkData :: Ptr Word8 -> IO (Array Int Word8)
mkData ptr = do
              (size :: Word8) <- (#peek struct kvm_run, io.size) ptr
              (offset :: Word64) <- (#peek struct kvm_run, io.data_offset) ptr
              let dataptr = plusPtr ptr (fromIntegral offset)
              mkArray (fromIntegral size) dataptr


peekKvmRunExitIo :: Ptr a -> IO Io
peekKvmRunExitIo ptr = Io <$> ((#peek struct kvm_run, io.direction) ptr >>= (return.mkDirection))
                                 <*> (#peek struct kvm_run, io.port) ptr
                                 <*> (#peek struct kvm_run, io.count) ptr
                                 <*> mkData (castPtr ptr)
