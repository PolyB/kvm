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

--     = SetupHeader {
--      setup_sects :: Word8 
--     ,root_flags :: Word16 
--     ,syssize :: Word32 
--     ,ram_size :: Word16 
--     ,vid_mode :: Word16 
--     ,root_dev :: Word16 
--     ,boot_flag :: Word16 
--     ,jump :: Word16 
--     ,header :: Word32 
--     ,version :: Word16 
--     ,realmode_swtch :: Word32 
--     ,start_sys_seg :: Word16 
--     ,kernel_version :: Word16 
--     ,type_of_loader :: Word8 
--     ,loadflags :: Word8 
--     ,setup_move_size :: Word16 
--     ,code32_start :: Word32 
--     ,ramdisk_image :: Word32 
--     ,ramdisk_size :: Word32 
--     ,bootsect_kludge :: Word32 
--     ,heap_end_ptr :: Word16 
--     ,ext_loader_ver :: Word8 
--     ,ext_loader_type :: Word8 
--     ,cmd_line_ptr :: Word32 
--     ,initrd_addr_max :: Word32 
--     ,kernel_alignment :: Word32 
--     ,relocatable_kernel :: Word8 
--     ,min_alignment :: Word8 
--     ,xloadflags :: Word16 
--     ,cmdline_size :: Word32 
--     ,hardware_subarch :: Word32 
--     ,hardware_subarch_data :: Word64 
--     ,payload_offset :: Word32 
--     ,payload_length :: Word32 
--     ,setup_data :: Word64 
--     ,pref_address :: Word64 
--     ,init_size :: Word32 
--     ,handover_offset :: Word32 
-- }
--     deriving (Show)
-- 
-- peekSetupHeader :: Ptr SetupHeader -> IO SetupHeader
-- peekSetupHeader ptr = SetupHeader   <$> (#peek struct setup_header, setup_sects) ptr
--                                     <*> (#peek struct setup_header, root_flags) ptr
--                                     <*> (#peek struct setup_header, syssize) ptr
--                                     <*> (#peek struct setup_header, ram_size) ptr
--                                     <*> (#peek struct setup_header, vid_mode) ptr
--                                     <*> (#peek struct setup_header, root_dev) ptr
--                                     <*> (#peek struct setup_header, boot_flag) ptr
--                                     <*> (#peek struct setup_header, jump) ptr
--                                     <*> (#peek struct setup_header, header) ptr
--                                     <*> (#peek struct setup_header, version) ptr
--                                     <*> (#peek struct setup_header, realmode_swtch) ptr
--                                     <*> (#peek struct setup_header, start_sys_seg) ptr
--                                     <*> (#peek struct setup_header, kernel_version) ptr
--                                     <*> (#peek struct setup_header, type_of_loader) ptr
--                                     <*> (#peek struct setup_header, loadflags) ptr
--                                     <*> (#peek struct setup_header, setup_move_size) ptr
--                                     <*> (#peek struct setup_header, code32_start) ptr
--                                     <*> (#peek struct setup_header, ramdisk_image) ptr
--                                     <*> (#peek struct setup_header, ramdisk_size) ptr
--                                     <*> (#peek struct setup_header, bootsect_kludge) ptr
--                                     <*> (#peek struct setup_header, heap_end_ptr) ptr
--                                     <*> (#peek struct setup_header, ext_loader_ver) ptr
--                                     <*> (#peek struct setup_header, ext_loader_type) ptr
--                                     <*> (#peek struct setup_header, cmd_line_ptr) ptr
--                                     <*> (#peek struct setup_header, initrd_addr_max) ptr
--                                     <*> (#peek struct setup_header, kernel_alignment) ptr
--                                     <*> (#peek struct setup_header, relocatable_kernel) ptr
--                                     <*> (#peek struct setup_header, min_alignment) ptr
--                                     <*> (#peek struct setup_header, xloadflags) ptr
--                                     <*> (#peek struct setup_header, cmdline_size) ptr
--                                     <*> (#peek struct setup_header, hardware_subarch) ptr
--                                     <*> (#peek struct setup_header, hardware_subarch_data) ptr
--                                     <*> (#peek struct setup_header, payload_offset) ptr
--                                     <*> (#peek struct setup_header, payload_length) ptr
--                                     <*> (#peek struct setup_header, setup_data) ptr
--                                     <*> (#peek struct setup_header, pref_address) ptr
--                                     <*> (#peek struct setup_header, init_size) ptr
--                                     <*> (#peek struct setup_header, handover_offset) ptr
-- 
-- pokeSetupHeader :: Ptr SetupHeader -> SetupHeader -> IO ()
-- pokeSetupHeader ptr s = (#poke struct setup_header, setup_sects) ptr (setup_sects s)
--                     >> (#poke struct setup_header, root_flags) ptr (root_flags s)
--                     >> (#poke struct setup_header, syssize) ptr (syssize s)
--                     >> (#poke struct setup_header, ram_size) ptr (ram_size s)
--                     >> (#poke struct setup_header, vid_mode) ptr (vid_mode s)
--                     >> (#poke struct setup_header, root_dev) ptr (root_dev s)
--                     >> (#poke struct setup_header, boot_flag) ptr (boot_flag s)
--                     >> (#poke struct setup_header, jump) ptr (jump s)
--                     >> (#poke struct setup_header, header) ptr (header s)
--                     >> (#poke struct setup_header, version) ptr (version s)
--                     >> (#poke struct setup_header, realmode_swtch) ptr (realmode_swtch s)
--                     >> (#poke struct setup_header, start_sys_seg) ptr (start_sys_seg s)
--                     >> (#poke struct setup_header, kernel_version) ptr (kernel_version s)
--                     >> (#poke struct setup_header, type_of_loader) ptr (type_of_loader s)
--                     >> (#poke struct setup_header, loadflags) ptr (loadflags s)
--                     >> (#poke struct setup_header, setup_move_size) ptr (setup_move_size s)
--                     >> (#poke struct setup_header, code32_start) ptr (code32_start s)
--                     >> (#poke struct setup_header, ramdisk_image) ptr (ramdisk_image s)
--                     >> (#poke struct setup_header, ramdisk_size) ptr (ramdisk_size s)
--                     >> (#poke struct setup_header, bootsect_kludge) ptr (bootsect_kludge s)
--                     >> (#poke struct setup_header, heap_end_ptr) ptr (heap_end_ptr s)
--                     >> (#poke struct setup_header, ext_loader_ver) ptr (ext_loader_ver s)
--                     >> (#poke struct setup_header, ext_loader_type) ptr (ext_loader_type s)
--                     >> (#poke struct setup_header, cmd_line_ptr) ptr (cmd_line_ptr s)
--                     >> (#poke struct setup_header, initrd_addr_max) ptr (initrd_addr_max s)
--                     >> (#poke struct setup_header, kernel_alignment) ptr (kernel_alignment s)
--                     >> (#poke struct setup_header, relocatable_kernel) ptr (relocatable_kernel s)
--                     >> (#poke struct setup_header, min_alignment) ptr (min_alignment s)
--                     >> (#poke struct setup_header, xloadflags) ptr (xloadflags s)
--                     >> (#poke struct setup_header, cmdline_size) ptr (cmdline_size s)
--                     >> (#poke struct setup_header, hardware_subarch) ptr (hardware_subarch s)
--                     >> (#poke struct setup_header, hardware_subarch_data) ptr (hardware_subarch_data s)
--                     >> (#poke struct setup_header, payload_offset) ptr (payload_offset s)
--                     >> (#poke struct setup_header, payload_length) ptr (payload_length s)
--                     >> (#poke struct setup_header, setup_data) ptr (setup_data s)
--                     >> (#poke struct setup_header, pref_address) ptr (pref_address s)
--                     >> (#poke struct setup_header, init_size) ptr (init_size s)
--                     >> (#poke struct setup_header, handover_offset) ptr (handover_offset s)

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
              in (#read_field header) >>= (\h -> when (h /= setupHeaderMagic) $ I.throw' ErrorBadKernelFile)

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
