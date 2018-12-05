{-# LANGUAGE TemplateHaskell #-}

module System.Linux.Kvm.IoCtl.Types.UserspaceMemoryRegion where

import Foreign.C.Types
import Foreign
import Foreign.Ptr
import Foreign.C
import Control.Lens

#include <linux/kvm.h>

data UserspaceMemoryRegion = UserspaceMemoryRegion {
  _slot :: Word32,
  _flags :: Word32,
  _guest_phys_addr :: Word64,
  _memory_size :: Word64,
  _userspace_addr :: Ptr ()
}
makeLenses ''UserspaceMemoryRegion

instance Storable UserspaceMemoryRegion where
  sizeOf _ = (#size struct kvm_userspace_memory_region)
  alignment _ = (#alignment struct kvm_userspace_memory_region)
  peek ptr = UserspaceMemoryRegion  <$> (#peek struct kvm_userspace_memory_region, slot) ptr 
                                    <*> (#peek struct kvm_userspace_memory_region, flags) ptr 
                                    <*> (#peek struct kvm_userspace_memory_region, guest_phys_addr) ptr 
                                    <*> (#peek struct kvm_userspace_memory_region, memory_size) ptr 
                                    <*> (#peek struct kvm_userspace_memory_region, userspace_addr) ptr 
  poke ptr (UserspaceMemoryRegion sl fl guest size user) =  (#poke struct kvm_userspace_memory_region, slot) ptr sl
                                                         >> (#poke struct kvm_userspace_memory_region, flags) ptr fl
                                                         >> (#poke struct kvm_userspace_memory_region, guest_phys_addr) ptr guest
                                                         >> (#poke struct kvm_userspace_memory_region, memory_size) ptr size
                                                         >> (#poke struct kvm_userspace_memory_region, userspace_addr) ptr user
