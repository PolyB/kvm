{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleContexts #-}

module System.Linux.Kvm.Components.Init.E820Entry where

import Foreign.Storable
import qualified Ether.Except as I
import Data.Word
import Control.Monad.IO.Class
import System.Linux.Kvm.Errors
import Foreign.Ptr
import System.Linux.Kvm.Components.Init.SetupHeader
import Control.Monad
import Foreign.Marshal.Array
import System.Linux.Kvm.Components.Init.BootParams

#include <asm/bootparam.h>

-- Based on linux/arch/x86/include/asm/e820/types.h
data E820Type = E820_Ram
              | E820_Reserved
              | E820_ACPI
              | E820_NVS
              | E820_Unusable
              | E820_Pmem
              | E820_Pram
              | E820_Unknown Word32

typeToWord :: E820Type -> Word32
typeToWord E820_Ram = 1
typeToWord E820_Reserved = 2
typeToWord E820_ACPI = 3
typeToWord E820_NVS = 4
typeToWord E820_Unusable = 5
typeToWord E820_Pmem = 7
typeToWord E820_Pram = 12
typeToWord (E820_Unknown a) = a

wordTotype :: Word32 -> E820Type
wordTotype 1 = E820_Ram
wordTotype 2 = E820_Reserved
wordTotype 3 = E820_ACPI
wordTotype 4 = E820_NVS
wordTotype 5 = E820_Unusable
wordTotype 7 = E820_Pmem
wordTotype 12 = E820_Pram
wordTotype a = E820_Unknown a

data E820Entry = E820Entry
    { addr :: Word64
    , size :: Word64
    , entrytype :: E820Type
    }

instance Storable E820Entry where
    sizeOf _ = (#size struct boot_e820_entry)
    alignment _ = (#alignment struct boot_e820_entry)
    peek ptr = E820Entry <$> (#peek struct boot_e820_entry, addr) ptr
                         <*> (#peek struct boot_e820_entry, size) ptr
                         <*> (wordTotype <$> (#peek struct boot_e820_entry, type) ptr)
    poke ptr (E820Entry addr size typ) = (#poke struct boot_e820_entry, addr) ptr addr
                                       >> (#poke struct boot_e820_entry, size) ptr size
                                       >> (#poke struct boot_e820_entry, type) ptr (typeToWord typ)


writeE820Entries :: (MonadIO m, MonadError m, MonadBootParams m) => [E820Entry] -> m ()
writeE820Entries ents = do
                            ptr <- bootParams
                            when (length ents > (#const E820_MAX_ENTRIES_ZEROPAGE)) $ throwE ErrorTooManyE820Entries "writeE820Entries"
                            liftIO $ (#poke struct boot_params, e820_entries) ptr ((fromIntegral $ length ents):: Word8)
                            liftIO $ pokeArray ((#ptr struct boot_params, e820_table) ptr) ents
