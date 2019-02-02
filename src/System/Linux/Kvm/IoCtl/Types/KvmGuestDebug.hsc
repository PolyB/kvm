{-# LANGUAGE TemplateHaskell #-}

module System.Linux.Kvm.IoCtl.Types.KvmGuestDebug where

import Foreign.C.Types
import Foreign
import Foreign.Ptr
import Foreign.C
import Data.Word

#include <linux/kvm.h>

data KvmGuestDebug = KvmGuestDebug Word32

instance Semigroup KvmGuestDebug where
 (KvmGuestDebug a) <> (KvmGuestDebug b) = KvmGuestDebug (a .|. b)

instance Monoid KvmGuestDebug where
 mempty = KvmGuestDebug 0

instance Storable KvmGuestDebug where
    sizeOf _ = (#size struct kvm_guest_debug)
    alignment _ = (#alignment struct kvm_guest_debug)
    peek ptr = KvmGuestDebug <$> (#peek struct kvm_guest_debug, control) ptr
    poke ptr (KvmGuestDebug v) = do
                                    fillBytes ptr 0 (#size struct kvm_guest_debug)
                                    (#poke struct kvm_guest_debug, control) ptr v
                

guestDbgEnable      = KvmGuestDebug (#const KVM_GUESTDBG_ENABLE)
guestDbgSinglestep  = KvmGuestDebug (#const KVM_GUESTDBG_SINGLESTEP)
guestDbgUseSwBp     = KvmGuestDebug (#const KVM_GUESTDBG_USE_SW_BP)
guestDbgUseHwBp     = KvmGuestDebug (#const KVM_GUESTDBG_USE_HW_BP)
guestDbgInjectDb    = KvmGuestDebug (#const KVM_GUESTDBG_INJECT_DB)
guestDbgInjectBp    = KvmGuestDebug (#const KVM_GUESTDBG_INJECT_BP)


