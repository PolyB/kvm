{-# LANGUAGE TemplateHaskell #-}

module System.Linux.Kvm.IoCtl.Types.KvmRun
(
  module System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunBase
 ,KvmRun(..), peekKvmRun, KvmRunExit(..)
) 
where

import Foreign.C.Types
import Foreign
import Foreign.Ptr
import Foreign.C
import Control.Lens
import System.Linux.Kvm.IoCtl.Types.Segment
import System.Linux.Kvm.IoCtl.Types.DTable
import System.Linux.Kvm.IoCtl.Types.InterruptMask
import System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunBase
import System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunExit.Unknown
import System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunExit.FailEntry
import System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunExit.Exception
import System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunExit.Io
import System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunExit.Debug
import System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunExit.Mmio
import System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunExit.Hypercall
import System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunExit.TprAccess
import System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunExit.InternalError
import System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunExit.Osi
import System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunExit.PaprHcall
import System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunExit.Epr
import System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunExit.SystemEvent
import System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunExit.IoapicEoi
import System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunExit.Hyperv

#include <linux/kvm.h>

data KvmRunExit
  = KvmRunExitUnknown Unknown
  | KvmRunExitFailEntry FailEntry
  | KvmRunExitException Exception
  | KvmRunExitIo Io
  | KvmRunExitDebug Debug
  | KvmRunExitMmio Mmio
  | KvmRunExitHypercall Hypercall
  | KvmRunExitTprAccess TprAccess
--   | KvmRunExitS390Sieic
--   | KvmRunExitS390Reset
--   | KvmRunExitS390UControl
  | KvmRunExitInternalError InternalError
  | KvmRunExitOsi Osi
  | KvmRunExitPaprHcall PaprHcall
--   | KvmRunExitS390Tsch
  | KvmRunExitEpr Epr
  | KvmRunExitSystemEvent SystemEvent
--  | KvmRunExitS390Stsi
  | KvmRunExitIoApicEoi IoapicEoi
  | KvmRunExitHyperv Hyperv
  | KvmRunExitHlt
  | KvmRunExitNotImplemented Word32
  deriving (Show, Eq)





data KvmRun = KvmRun KvmRunBase KvmRunExit
  deriving (Show, Eq)

peekKvmRunExit :: Word32 -> Ptr KvmRun -> IO KvmRunExit
peekKvmRunExit (#const KVM_EXIT_UNKNOWN)        = fmap KvmRunExitUnknown . peekKvmRunExitUnknown
peekKvmRunExit (#const KVM_EXIT_FAIL_ENTRY)     = fmap KvmRunExitFailEntry . peekKvmRunExitFailEntry
peekKvmRunExit (#const KVM_EXIT_EXCEPTION)      = fmap KvmRunExitException . peekKvmRunExitException
peekKvmRunExit (#const KVM_EXIT_IO)             = fmap KvmRunExitIo .  peekKvmRunExitIo
peekKvmRunExit (#const KVM_EXIT_DEBUG)          = fmap KvmRunExitDebug . peekKvmRunExitDebug
peekKvmRunExit (#const KVM_EXIT_MMIO)           = fmap KvmRunExitMmio . peekKvmRunExitMmio
peekKvmRunExit (#const KVM_EXIT_HYPERCALL)      = fmap KvmRunExitHypercall . peekKvmRunExitHypercall
peekKvmRunExit (#const KVM_EXIT_TPR_ACCESS)     = fmap KvmRunExitTprAccess . peekKvmRunExitTprAccess
-- peekKvmRunExit (#const KVM_EXIT_S390_SIEIC)     = fmap KvmRunExitS390Sieic . peekKvmRunExitS390Sieic
-- peekKvmRunExit (#const KVM_EXIT_S390_RESET)     = fmap KvmRunExitS390Reset . peekKvmRunExitS390Reset
-- peekKvmRunExit (#const KVM_EXIT_S390_UCONTROL)  = fmap KvmRunExitS390UControl . peekKvmRunExitS390UControl
peekKvmRunExit (#const KVM_EXIT_INTERNAL_ERROR) = fmap KvmRunExitInternalError . peekKvmRunExitInternalError
peekKvmRunExit (#const KVM_EXIT_OSI)            = fmap KvmRunExitOsi . peekKvmRunExitOsi
peekKvmRunExit (#const KVM_EXIT_PAPR_HCALL)     = fmap KvmRunExitPaprHcall . peekKvmRunExitPaprHcall
-- peekKvmRunExit (#const KVM_EXIT_S390_TSCH)      = fmap KvmRunExitS390Tsch . peekKvmRunExitS390Tsch
peekKvmRunExit (#const KVM_EXIT_EPR)            = fmap KvmRunExitEpr . peekKvmRunExitEpr
peekKvmRunExit (#const KVM_EXIT_SYSTEM_EVENT)   = fmap KvmRunExitSystemEvent . peekKvmRunExitSystemEvent
-- peekKvmRunExit (#const KVM_EXIT_S390_STSI)      = fmap KvmRunExitS390Stsi . peekKvmRunExitS390Stsi
peekKvmRunExit (#const KVM_EXIT_IOAPIC_EOI)     = fmap KvmRunExitIoApicEoi . peekKvmRunExitIoapicEoi
peekKvmRunExit (#const KVM_EXIT_HYPERV)         = fmap KvmRunExitHyperv . peekKvmRunExitHyperv
peekKvmRunExit (#const KVM_EXIT_HLT)            = const $ return KvmRunExitHlt
peekKvmRunExit x = const $ return $ KvmRunExitNotImplemented x



peekKvmRun :: Ptr KvmRun -> IO KvmRun
peekKvmRun ptr = do
                  base <- peekKvmRunBase (castPtr ptr)
                  exit <- peekKvmRunExit (base^.exit_reason) ptr
                  return $ KvmRun base exit

