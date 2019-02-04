{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module System.Linux.Kvm.KvmM.Cpu 
  (
    -- * ReExports
    module System.Linux.Kvm.IoCtl.Types.Regs
   ,module System.Linux.Kvm.IoCtl.Types.SRegs
    -- * Cpu Monad
   ,Cpu
   ,CpuT
   ,runCpuT
   ,cpuContinue
   ,cpuContinueUntilVmEnd
   ,MonadCpu
   -- * Cpu Actions
   ,translateAddr
   -- * Cpu Properties
   ,getExitReason
   ,regs
   ,sregs
   ,guest_debug
  ) 
where

import System.Linux.Kvm.Errors
import Control.Lens
import System.Linux.Kvm.KvmM.Vm
import System.Linux.Kvm.IoCtl
import System.Linux.Kvm.IoCtl.Types
import System.Linux.Kvm.IoCtl.Types.Regs
import System.Linux.Kvm.IoCtl.Types.SRegs
import System.Linux.Kvm.IoCtl.Types.KvmRun
import Foreign.Ptr
import System.Linux.Kvm.IoCtl.Types.KvmGuestDebug
import System.Linux.Kvm.KvmM.Kvm
import Control.Monad.IO.Class
import qualified Ether.State as I
import qualified Ether.TagDispatch as I
import Control.Monad
import Data.Word


data Cpu = Cpu
  {
    _cpufd :: VcpuFd
   ,_regs :: Regs
   ,_sregs :: SRegs
   ,_regs_before :: Regs
   ,_sregs_before :: SRegs
   ,_kvm_run :: Ptr KvmRun
   ,_guest_debug :: KvmGuestDebug
   ,_guest_debug_before :: KvmGuestDebug
  }
makeLenses ''Cpu

type CpuT m = I.StateT' Cpu m

type MonadCpu m = I.MonadState' Cpu m

-- | Continue the Vm, calls /KVM_RUN/ behind and update the 'CpuT' monad with the new informations
cpuContinue:: (MonadError m, MonadIO m, MonadCpu m) => m ()
cpuContinue = do
                  fd <- I.gets' _cpufd
                  cregs <- I.gets' _regs
                  csregs <- I.gets' _sregs
                  debug <- I.gets' _guest_debug
                  oregs <- I.gets' _regs_before
                  osregs <- I.gets' _sregs_before
                  odebug <- I.gets' _guest_debug_before
                  when (cregs /= oregs) $ liftIO $ setRegs fd cregs
                  when (csregs /= osregs) $ liftIO $ setSRegs fd csregs
                  when (debug /= odebug) $ liftIO $ setGuestDebug fd debug
                  execIO $ runKvm fd
                  newregs <- liftIO $ getRegs fd
                  newsregs <- liftIO $ getSRegs fd
                  I.tagAttach @Cpu $ do
                    regs .= newregs
                    sregs .= newsregs
                    regs_before .= newregs
                    sregs_before .= newsregs

cpuContinueUntilVmEnd::(MonadError m, MonadIO m, MonadCpu m, MonadVm m) => (KvmRunExit -> KvmRunT m ()) -> m ()
cpuContinueUntilVmEnd act = doContinueVm >>= (\cont -> if cont then cpuContinue >> do
                                                                                    (exit, base) <- getExitReason
                                                                                    I.evalStateT' (act exit) base
                                                                                    cpuContinueUntilVmEnd act else return ())

-- | Create a CPU and run it inside a 'CpuT' 'MonadTransformer'
runCpuT :: (MonadError m, MonadVm m, MonadKvm m, MonadIO m) => CpuT m a -> m a
runCpuT act = do
              vm <- vmfd 
              fd <- execIO $ createVCPU vm 0 -- TODO : modify if multiple cpus is needed
              kvmfd <- kvmfd
              kvmrunptr <- liftIO $ allocKvmRun kvmfd fd
              newregs <- liftIO $ getRegs fd
              newsregs <- liftIO $ getSRegs fd
              I.evalStateT' act $ Cpu {
                _cpufd = fd
               ,_regs = newregs
               ,_regs_before = newregs
               ,_sregs =  newsregs
               ,_sregs_before =  newsregs
               ,_kvm_run = kvmrunptr
               ,_guest_debug_before = mempty
               ,_guest_debug = mempty
              }

-- | Returns the exit reasons of the CPU
getExitReason:: (MonadCpu m, MonadIO m) => m (KvmRunExit, KvmRunBase)
getExitReason = do
                  kvmrunptr <- I.gets' _kvm_run
                  (KvmRun base exit) <- liftIO $ peekKvmRun kvmrunptr
                  return (exit, base)

translateAddr :: (MonadCpu m, MonadIO m) => Word64 -> m Word64
translateAddr addr = do
                        fd <- I.gets' _cpufd
                        liftIO $ kvmTranslate fd addr

    
                        

