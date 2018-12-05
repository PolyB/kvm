{-# LANGUAGE TemplateHaskell #-}

module System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunBase where

import Foreign.C.Types
import Foreign
import Foreign.Ptr
import Foreign.C
import Control.Lens
import System.Linux.Kvm.IoCtl.Types.Segment
import System.Linux.Kvm.IoCtl.Types.DTable
import System.Linux.Kvm.IoCtl.Types.InterruptMask

#include <linux/kvm.h>

data KvmRunBase = KvmRunBase
  {
  _request_interrupt_window :: Word8
 ,_immediate_exit :: Word8
 ,_exit_reason :: Word32
 ,_ready_for_interrupt_injection :: Word8
 ,_if_flag :: Word8
 ,_flags :: Word16
 ,_cr8b :: Word64
 ,_apic_baseb :: Word64
 -- TODO : S390 ?
  }
  deriving (Show, Eq)
makeLenses ''KvmRunBase

peekKvmRunBase:: Ptr KvmRunBase -> IO KvmRunBase
peekKvmRunBase ptr = KvmRunBase <$> (#peek struct kvm_run, request_interrupt_window) ptr
                                <*> (#peek struct kvm_run, immediate_exit) ptr
                                <*> (#peek struct kvm_run, exit_reason) ptr
                                <*> (#peek struct kvm_run, ready_for_interrupt_injection) ptr
                                <*> (#peek struct kvm_run, if_flag) ptr
                                <*> (#peek struct kvm_run, flags) ptr
                                <*> (#peek struct kvm_run, cr8) ptr
                                <*> (#peek struct kvm_run, apic_base) ptr
pokeKvmRunBase :: Ptr KvmRunBase -> KvmRunBase -> IO ()
pokeKvmRunBase ptr v = (#poke struct kvm_run, request_interrupt_window) ptr (v^.request_interrupt_window)
                    >> (#poke struct kvm_run, immediate_exit) ptr (v^.immediate_exit)
                    >> (#poke struct kvm_run, exit_reason) ptr (v^.exit_reason)
                    >> (#poke struct kvm_run, ready_for_interrupt_injection) ptr (v^.ready_for_interrupt_injection)
                    >> (#poke struct kvm_run, if_flag) ptr (v^.if_flag)
                    >> (#poke struct kvm_run, flags) ptr (v^.flags)
                    >> (#poke struct kvm_run, cr8) ptr (v^.cr8b)
                    >> (#poke struct kvm_run, apic_base) ptr (v^.apic_baseb)
