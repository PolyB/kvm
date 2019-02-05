{-# LANGUAGE TemplateHaskell #-}

module System.Linux.Kvm.IoCtl.Types.Cpuid2 where

#include <linux/kvm.h>

import Data.Word
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Ptr

data Cpuid2Entry = Cpuid2Entry {
                        function :: Word32
                       ,index :: Word32
                       ,flags :: Word32
                       ,eax :: Word32
                       ,ebx :: Word32
                       ,ecx :: Word32
                       ,edx :: Word32
                    }
                    deriving (Show)
data Cpuid2 = Cpuid2 [Cpuid2Entry]

sizeOfCpuid :: Cpuid2 -> Int
sizeOfCpuid (Cpuid2 entries) = (#size struct kvm_cpuid2) + (length entries) * (#size struct kvm_cpuid_entry2)


pokeCpuid :: Ptr Cpuid2 -> Cpuid2 -> IO ()
pokeCpuid ptr (Cpuid2 entries) = (#poke struct kvm_cpuid2, nent) ptr ((fromIntegral $ length entries)::Word32)
                                >> pokeArray ((#ptr struct kvm_cpuid2, entries) ptr) entries

mkCpuidEntry :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Cpuid2Entry
mkCpuidEntry fun a b c d = Cpuid2Entry { function = fun, index = 0, flags = 0, eax = a, ebx = b, ecx = c, edx = d }

instance Storable Cpuid2Entry where
    sizeOf _ = (#size struct kvm_cpuid_entry2)
    alignment _ = (#alignment struct kvm_cpuid_entry2)
    peek ptr = Cpuid2Entry <$> (#peek struct kvm_cpuid_entry2, function) ptr
                           <*> (#peek struct kvm_cpuid_entry2, index) ptr
                           <*> (#peek struct kvm_cpuid_entry2, flags) ptr
                           <*> (#peek struct kvm_cpuid_entry2, eax) ptr
                           <*> (#peek struct kvm_cpuid_entry2, ebx) ptr
                           <*> (#peek struct kvm_cpuid_entry2, ecx) ptr
                           <*> (#peek struct kvm_cpuid_entry2, edx) ptr
    poke ptr (Cpuid2Entry fun ind fl ea eb ec ed) = 
        (#poke struct kvm_cpuid_entry2, function) ptr fun
        >> (#poke struct kvm_cpuid_entry2, index) ptr ind
        >> (#poke struct kvm_cpuid_entry2, flags) ptr fl
        >> (#poke struct kvm_cpuid_entry2, eax) ptr ea
        >> (#poke struct kvm_cpuid_entry2, ebx) ptr eb
        >> (#poke struct kvm_cpuid_entry2, ecx) ptr ec
        >> (#poke struct kvm_cpuid_entry2, edx) ptr ed

instance Semigroup Cpuid2 where
    (Cpuid2 a) <> (Cpuid2 b) = Cpuid2 $ a <> b

instance Monoid Cpuid2 where
    mempty = Cpuid2 []

