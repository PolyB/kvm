module System.Linux.Kvm 
  (
    VMInstance(..)
   ,startVm
   ,module System.Linux.Kvm.KvmM
   ,module System.Linux.Kvm.IoCtl.Types.KvmRun

  )

where

import Control.Lens
import Control.Monad.Trans.Class
import System.Linux.Kvm.IoCtl.Types.KvmRun
import System.Linux.Kvm.KvmM

data VMInstance a = VMInstance
  {
     setup :: VmM ()
    ,startup :: CpuM ()
    ,handle :: KvmRunExit -> KvmRunBase -> CpuM ()
    ,stop :: CpuM a
  }

whileM :: Monad m => m Bool -> m ()
whileM a = a >>= (\x -> if x then whileM a else return ())

doContinue:: CpuM Bool
doContinue = lift $ use continueVm

startVm :: VMInstance a -> IO a
startVm inst = runKvmM $ runVmM $ setup inst >> runCpuM (do
                                                            startup inst
                                                            whileM (cpuContinue >> getExitReason >>= uncurry (handle inst) >> doContinue)
                                                            stop inst)
