{-# LANGUAGE TemplateHaskell #-}

module System.Linux.Kvm.IoCtl.Types.SRegs where

import Foreign.C.Types
import Foreign
import Foreign.Ptr
import Foreign.C
import Control.Lens
import System.Linux.Kvm.IoCtl.Types.Segment
import System.Linux.Kvm.IoCtl.Types.DTable
import System.Linux.Kvm.IoCtl.Types.InterruptMask

#include <linux/kvm.h>

data SRegs = SRegs {
  _cs, _ds, _es, _fs, _gs, _ss :: Segment
 ,_tr, _ldt :: Segment
 ,_gdt, _idt :: DTable
 ,_cr0, _cr2, _cr3, _cr4, _cr8 :: Word64
 ,_efer :: Word64
 ,_apic_base :: Word64
 ,_interrupt_bitmap :: InterruptMask
}
  deriving (Eq, Show)
makeLenses ''SRegs

instance Storable SRegs where
  sizeOf _ = (#size struct kvm_sregs)
  alignment _ = (#alignment struct kvm_sregs)
  peek ptr = SRegs <$> (#peek struct kvm_sregs, cs) ptr 
                   <*> (#peek struct kvm_sregs, ds) ptr 
                   <*> (#peek struct kvm_sregs, es) ptr 
                   <*> (#peek struct kvm_sregs, fs) ptr 
                   <*> (#peek struct kvm_sregs, gs) ptr 
                   <*> (#peek struct kvm_sregs, ss) ptr 
                   <*> (#peek struct kvm_sregs, tr) ptr 
                   <*> (#peek struct kvm_sregs, ldt) ptr 
                   <*> (#peek struct kvm_sregs, gdt) ptr 
                   <*> (#peek struct kvm_sregs, idt) ptr 
                   <*> (#peek struct kvm_sregs, cr0) ptr 
                   <*> (#peek struct kvm_sregs, cr2) ptr 
                   <*> (#peek struct kvm_sregs, cr3) ptr 
                   <*> (#peek struct kvm_sregs, cr4) ptr 
                   <*> (#peek struct kvm_sregs, cr8) ptr 
                   <*> (#peek struct kvm_sregs, efer) ptr 
                   <*> (#peek struct kvm_sregs, apic_base) ptr 
                   <*> (#peek struct kvm_sregs, interrupt_bitmap) ptr 

  poke ptr regs =  (#poke struct kvm_sregs, cs) ptr (regs^.cs)
                >> (#poke struct kvm_sregs, ds) ptr (regs^.ds)
                >> (#poke struct kvm_sregs, es) ptr (regs^.es)
                >> (#poke struct kvm_sregs, fs) ptr (regs^.fs)
                >> (#poke struct kvm_sregs, gs) ptr (regs^.gs)
                >> (#poke struct kvm_sregs, ss) ptr (regs^.ss)
                >> (#poke struct kvm_sregs, tr) ptr (regs^.tr)
                >> (#poke struct kvm_sregs, ldt) ptr (regs^.ldt)
                >> (#poke struct kvm_sregs, gdt) ptr (regs^.gdt)
                >> (#poke struct kvm_sregs, idt) ptr (regs^.idt)
                >> (#poke struct kvm_sregs, cr0) ptr (regs^.cr0)
                >> (#poke struct kvm_sregs, cr2) ptr (regs^.cr2)
                >> (#poke struct kvm_sregs, cr3) ptr (regs^.cr3)
                >> (#poke struct kvm_sregs, cr4) ptr (regs^.cr4)
                >> (#poke struct kvm_sregs, cr8) ptr (regs^.cr8)
                >> (#poke struct kvm_sregs, efer) ptr (regs^.efer)
                >> (#poke struct kvm_sregs, apic_base) ptr (regs^.apic_base)
                >> (#poke struct kvm_sregs, interrupt_bitmap) ptr (regs^.interrupt_bitmap)
emptySRegs:: SRegs
emptySRegs = SRegs {
              _cs = emptySegment
             ,_ds = emptySegment
             ,_es = emptySegment
             ,_fs = emptySegment
             ,_gs = emptySegment
             ,_ss = emptySegment
             ,_tr = emptySegment
             ,_ldt = emptySegment
             ,_gdt = emptyDtable
             ,_idt = emptyDtable
             ,_cr0 = 0, _cr2 = 0, _cr3 = 0, _cr4 = 0, _cr8 = 0
             ,_efer = 0
             ,_apic_base = 0
             ,_interrupt_bitmap = emptyInterruptMask
                }
