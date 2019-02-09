{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module System.Linux.Kvm.KvmM.Cpu 
  (
    -- * ReExports
    module System.Linux.Kvm.IoCtl.Types.Regs
   ,module System.Linux.Kvm.IoCtl.Types.SRegs
   ,module System.Linux.Kvm.Utils.LVar
    -- * Cpu Monad
   ,Cpu
   ,CpuT
   ,runCpuT
   ,cpuContinue
   ,cpuContinueUntilVmEnd
   ,MonadCpu
   -- * Cpu Actions
   ,translateAddr
   ,setCpuid
   -- * Cpu Properties
   ,getExitReason
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
import System.Linux.Kvm.IoCtl.Types.Cpuid2
import Foreign.Ptr
import System.Linux.Kvm.IoCtl.Types.KvmGuestDebug
import System.Linux.Kvm.KvmM.Kvm
import Control.Monad.IO.Class
import qualified Ether.State as I
import qualified Ether.TagDispatch as I
import Control.Monad
import Data.Word
import System.Linux.Kvm.Utils.LVar

data Cpu = Cpu
  {
    cpufd :: VcpuFd
   ,regs :: LVar Regs
   ,sregs :: LVar SRegs
   ,kvm_run :: Ptr KvmRun
   ,guest_debug :: LVar KvmGuestDebug
  }

type CpuT m = I.StateT' Cpu m

type MonadCpu m = I.MonadState' Cpu m


-- | Continue the Vm, calls /KVM_RUN/ behind and update the 'CpuT' monad with the new informations
cpuContinue:: (MonadError m, MonadIO m, MonadCpu m) => m ()
cpuContinue = do
                fd <- I.gets' cpufd
                lflush @Regs
                lflush @SRegs
                execIO $ runKvm fd

-- | call `cpuContinue` and repeat the given monad while the cpu should continue (until `vmStop`)
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
                cpufd = fd
               ,regs = Nothing
               ,sregs =  Nothing
               ,kvm_run = kvmrunptr
               ,guest_debug = Nothing
              }

-- | Returns the exit reasons of the CPU
getExitReason:: (MonadCpu m, MonadIO m) => m (KvmRunExit, KvmRunBase)
getExitReason = do
                  kvmrunptr <- I.gets' kvm_run
                  (KvmRun base exit) <- liftIO $ peekKvmRun kvmrunptr
                  return (exit, base)

translateAddr :: (MonadCpu m, MonadIO m) => Word64 -> m Word64
translateAddr addr = do
                        fd <- I.gets' cpufd
                        liftIO $ kvmTranslate fd addr

setCpuid :: (MonadCpu m, MonadIO m, MonadError m) => Cpuid2 -> m ()
setCpuid cpuid = do
                    fd <- I.gets' cpufd
                    execIO $ kvmSetCpuid fd cpuid


instance (MonadCpu m, MonadIO m) => LazyCpuVar m Regs where
    lread = I.gets' regs
    lwrite v = I.modify' $ \c -> c { regs = v }
    vread = do
                fd <- I.gets' cpufd
                liftIO $ getRegs fd

    vwrite v = do
                fd <- I.gets' cpufd
                liftIO $ setRegs fd v

instance (MonadCpu m, MonadIO m) => LazyCpuVar m SRegs where
    lread = I.gets' sregs
    lwrite v = I.modify' $ \c -> c { sregs = v }
    vread = do
                fd <- I.gets' cpufd
                liftIO $ getSRegs fd

    vwrite v = do
                fd <- I.gets' cpufd
                liftIO $ setSRegs fd v

instance (MonadCpu m, MonadIO m) => LazyCpuVar m KvmGuestDebug where
    lread = I.gets' guest_debug
    lwrite v = I.modify' $ \c -> c { guest_debug = v }
    vread = return mempty

    vwrite v = do
                fd <- I.gets' cpufd
                liftIO $ setGuestDebug fd v
