module System.Linux.Kvm 
  (
  -- * ReExports
    module System.Linux.Kvm.KvmM
   ,module System.Linux.Kvm.IoCtl.Types.KvmRun
 -- * Vm control
   ,VMInstance(..)
   ,startVm

  )

where

import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import System.Linux.Kvm.IoCtl.Types.KvmRun
import System.Linux.Kvm.KvmM

-- | The 'VMInstance' struct holds user handlers
data VMInstance m a = VMInstance
  {
     setup :: VmM m () -- ^ This is executed when the vm is created, can be used to allocate startup memory to the Vm
    ,startup :: CpuM m () -- ^ This is executed when the cpu is created, can be used to initialize (system) registers
    ,handle :: KvmRunExit -> KvmRunBase -> CpuM m () -- ^ This is called each time the vm exits, the reason is in 'KvmRunExit'
    ,stop :: CpuM m a -- ^ This is used to stop the vm, can be used to clean memory or others resources
  }

whileM :: Monad m => m Bool -> m ()
whileM a = a >>= (\x -> if x then whileM a else return ())

doContinue:: Monad m => CpuM m Bool
doContinue = lift $ use continueVm

startVm :: MonadIO m => VMInstance m a -> m a
startVm inst = runKvmM $ runVmM $ setup inst >> runCpuM (do
                                                            startup inst
                                                            whileM (cpuContinue >> getExitReason >>= uncurry (handle inst) >> doContinue)
                                                            stop inst)
