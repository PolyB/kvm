{-# LANGUAGE TemplateHaskell #-}

module System.Linux.Kvm.IoCtl.Types.Regs where

import Foreign.C.Types
import Foreign
import Foreign.Ptr
import Foreign.C
import Control.Lens

#include <linux/kvm.h>

data Regs = Regs {
   _rax, _rbx, _rcx, _rdx :: Word64
  ,_rsi, _rdi, _rsp, _rbp :: Word64
  ,_r8 , _r9 , _r10, _r11 :: Word64
  ,_r12, _r13, _r14, _r15 :: Word64
  ,_rip, _rflags :: Word64
}
  deriving (Show, Eq)
makeLenses ''Regs

emptyRegs :: Regs
emptyRegs = Regs 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

instance Storable Regs where
  sizeOf _ = (#size struct kvm_regs)
  alignment _ = (#alignment struct kvm_regs)
  peek ptr = Regs <$> (#peek struct kvm_regs, rax) ptr 
                  <*> (#peek struct kvm_regs, rbx) ptr 
                  <*> (#peek struct kvm_regs, rcx) ptr 
                  <*> (#peek struct kvm_regs, rdx) ptr 
                  <*> (#peek struct kvm_regs, rsi) ptr 
                  <*> (#peek struct kvm_regs, rdi) ptr 
                  <*> (#peek struct kvm_regs, rsp) ptr 
                  <*> (#peek struct kvm_regs, rbp) ptr 
                  <*> (#peek struct kvm_regs, r8) ptr 
                  <*> (#peek struct kvm_regs, r9) ptr 
                  <*> (#peek struct kvm_regs, r10) ptr 
                  <*> (#peek struct kvm_regs, r11) ptr 
                  <*> (#peek struct kvm_regs, r12) ptr 
                  <*> (#peek struct kvm_regs, r13) ptr 
                  <*> (#peek struct kvm_regs, r14) ptr 
                  <*> (#peek struct kvm_regs, r15) ptr 
                  <*> (#peek struct kvm_regs, rip) ptr 
                  <*> (#peek struct kvm_regs, rflags) ptr 

  poke ptr regs =  (#poke struct kvm_regs, rax) ptr (regs^.rax)
                >> (#poke struct kvm_regs, rbx) ptr (regs^.rbx)
                >> (#poke struct kvm_regs, rcx) ptr (regs^.rcx)
                >> (#poke struct kvm_regs, rdx) ptr (regs^.rdx)
                >> (#poke struct kvm_regs, rsi) ptr (regs^.rsi)
                >> (#poke struct kvm_regs, rdi) ptr (regs^.rdi)
                >> (#poke struct kvm_regs, rsp) ptr (regs^.rsp)
                >> (#poke struct kvm_regs, rbp) ptr (regs^.rbp)
                >> (#poke struct kvm_regs, r8) ptr (regs^.r8)
                >> (#poke struct kvm_regs, r9) ptr (regs^.r9)
                >> (#poke struct kvm_regs, r10) ptr (regs^.r10)
                >> (#poke struct kvm_regs, r11) ptr (regs^.r11)
                >> (#poke struct kvm_regs, r12) ptr (regs^.r12)
                >> (#poke struct kvm_regs, r13) ptr (regs^.r13)
                >> (#poke struct kvm_regs, r14) ptr (regs^.r14)
                >> (#poke struct kvm_regs, r15) ptr (regs^.r15)
                >> (#poke struct kvm_regs, rip) ptr (regs^.rip)
                >> (#poke struct kvm_regs, rflags) ptr (regs^.rflags)
