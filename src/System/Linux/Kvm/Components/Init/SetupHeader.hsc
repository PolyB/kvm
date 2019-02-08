{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Linux.Kvm.Components.Init.SetupHeader where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Data.Word
import System.Linux.Kvm.Components.Init.BootParams
import qualified Ether.Reader as I
import System.Linux.Kvm.Errors
import Control.Monad.IO.Class
import qualified Ether.Except as I
import Control.Monad
import Data.Bits

#include <asm/bootparam.h>

#define hsc_read_field(F)                   \
    {                                       \
        hsc_printf("(getP ");               \
        hsc_peek(struct setup_header, F);   \
        hsc_printf(")");                    \
    }

#define hsc_write_field(F)                  \
    {                                       \
        hsc_printf("(setP ");               \
        hsc_poke(struct setup_header, F);   \
        hsc_printf(")");                    \
    }

data SetupHeader

data LoadFlags = LoadFlags Word8

instance Semigroup LoadFlags where
    (LoadFlags a) <> (LoadFlags b) = LoadFlags (a .|. b)

instance Monoid LoadFlags where
    mempty = LoadFlags 0

getP :: (MonadBootParams m, MonadIO m, Storable a) => (Ptr SetupHeader -> IO a) -> m a
getP peeker = do
        (bootparamsPtr :: Ptr BootParams) <- I.ask'
        let setupHeaderPtr = (#ptr struct boot_params, hdr) bootparamsPtr
        liftIO $ peeker setupHeaderPtr

setP :: (MonadBootParams m, MonadIO m, Storable a) => (Ptr SetupHeader -> a -> IO ()) -> a -> m ()
setP poker v = do
        (bootparamsPtr :: Ptr BootParams) <- I.ask'
        let setupHeaderPtr = (#ptr struct boot_params, hdr) bootparamsPtr
        liftIO $ poker setupHeaderPtr v
                    

checkHeader:: (MonadBootParams m, MonadIO m, MonadError m) => m ()
checkHeader = let setupHeaderMagic = 0x53726448 ::Word32 -- equivalent to "HdrS", see linux/Documentation/x86/boot.txt
              in (#read_field header) >>= (\h -> when (h /= setupHeaderMagic) $ throwE ErrorBadKernelFile "checkHeader")

getBootProtocol:: (MonadBootParams m, MonadIO m) => m (Word16)
getBootProtocol = (#read_field version)

getSetupBinSize:: (MonadBootParams m, MonadIO m) => m (Int)
getSetupBinSize = do 
                    (sects :: Word8) <- (#read_field setup_sects)
                    return (512 * (1 + (if sects == 0 then 4 else fromIntegral sects)))

setLoaderType :: (MonadBootParams m, MonadIO m) => Word8 -> m ()
setLoaderType = (#write_field type_of_loader)

setHeapEndPtr :: (MonadBootParams m, MonadIO m) => Word16 -> m ()
setHeapEndPtr = (#write_field heap_end_ptr)

setLoadFlags :: (MonadBootParams m, MonadIO m) => LoadFlags -> m ()
setLoadFlags (LoadFlags x) = (#write_field loadflags) x

setCmdLinePtr :: (MonadBootParams m, MonadIO m) => Word32 -> m ()
setCmdLinePtr = (#write_field cmd_line_ptr)

getInitrdAddrMax :: (MonadBootParams m, MonadIO m) => m Word32
getInitrdAddrMax = (#read_field initrd_addr_max)

setRamDiskImage :: (MonadBootParams m, MonadIO m) => Word32 -> m ()
setRamDiskImage = (#write_field ramdisk_image)

setRamDiskSize :: (MonadBootParams m, MonadIO m) => Word32 -> m ()
setRamDiskSize = (#write_field ramdisk_size)

setupHeaderSize :: Int
setupHeaderSize = (#size struct setup_header)

bootParamsOffset :: Int
bootParamsOffset = (#offset struct boot_params, hdr)

getSetupHeader :: Ptr BootParams -> Ptr SetupHeader
getSetupHeader = (#ptr struct boot_params, hdr)




loadFlagsLoadedHigh :: LoadFlags
loadFlagsLoadedHigh = LoadFlags (#const LOADED_HIGH)

loadFlagsKaslr :: LoadFlags
loadFlagsKaslr = LoadFlags (#const KASLR_FLAG)

loadFlagsQuiet :: LoadFlags
loadFlagsQuiet = LoadFlags (#const QUIET_FLAG)

loadFlagsKeepSegments :: LoadFlags
loadFlagsKeepSegments = LoadFlags (#const KEEP_SEGMENTS)

loadFlagsCanUseHeap :: LoadFlags
loadFlagsCanUseHeap = LoadFlags (#const CAN_USE_HEAP)
