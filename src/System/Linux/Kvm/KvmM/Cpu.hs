{-# LANGUAGE TemplateHaskell #-}
module System.Linux.Kvm.KvmM.Cpu 
  (
    CpuMInternal()
   ,regs
   ,sregs
   ,CpuM
   ,runCpuM
   ,cpuContinue
   ,getExitReason
   ,module System.Linux.Kvm.IoCtl.Types.Regs
   ,module System.Linux.Kvm.IoCtl.Types.SRegs
  ) 
where

import Control.Lens
import System.Linux.Kvm.KvmM.Vm
import System.Linux.Kvm.KvmM.Utils
import System.Linux.Kvm.IoCtl
import System.Linux.Kvm.IoCtl.Types
import System.Linux.Kvm.IoCtl.Types.Regs
import System.Linux.Kvm.IoCtl.Types.SRegs
import Control.Monad.State.Strict
import Data.Void
import System.Linux.Kvm.IoCtl.Types.KvmRun
import Foreign.Ptr
import System.Linux.Kvm.KvmM.Kvm


data CpuMInternal = CpuMInternal
  {
    _cpufd :: VcpuFd
   ,_regs :: Regs
   ,_sregs :: SRegs
   ,_regs_before :: Regs
   ,_sregs_before :: SRegs
   ,_kvm_run :: Ptr KvmRun
  }
makeLenses ''CpuMInternal

type CpuM = StateT CpuMInternal VmM

cpuContinue:: CpuM ()
cpuContinue = do
                  fd <- use cpufd
                  cregs <- use regs
                  csregs <- use sregs
                  oregs <- use regs_before
                  osregs <- use sregs_before
                  when (cregs /= oregs) $ liftIO $ setRegs fd cregs
                  when (csregs /= osregs) $ liftIO $ setSRegs fd csregs
                  liftIO $ runKvm fd
                  newregs <- liftIO $ getRegs fd
                  newsregs <- liftIO $ getSRegs fd
                  regs .= newregs
                  sregs .= newsregs
                  regs_before .= newregs
                  sregs_before .= newsregs

runCpuM :: CpuM a -> VmM a
runCpuM act = do
              vm <- use vmfd 
              fd <- liftIO $ createVCPU vm 0 -- TODO : modify if multiple cpus is needed
              kvmfd <- lift $ use kvmfd
              kvmrunptr <- liftIO $ allocKvmRun kvmfd fd
              newregs <- liftIO $ getRegs fd
              newsregs <- liftIO $ getSRegs fd
              evalStateT act $ CpuMInternal {
                _cpufd = fd
               ,_regs = newregs
               ,_regs_before = newregs
               ,_sregs =  newsregs
               ,_sregs_before =  newsregs
               ,_kvm_run = kvmrunptr
              }

getExitReason:: CpuM (KvmRunExit, KvmRunBase)
getExitReason = do
                  kvmrunptr <- use kvm_run
                  (KvmRun base exit) <- liftIO $ peekKvmRun kvmrunptr
                  return (exit, base)
