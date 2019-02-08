{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Monad.IO.Class
import System.Linux.Kvm
import System.Linux.Kvm.Components.Init
import Data.Word
import System.Linux.Kvm.Components.Ram
import System.Linux.Kvm.Debug
import System.Linux.Kvm.ExitHandler
import System.Linux.Kvm.ExitHandler.Io
import System.Linux.Kvm.IoCtl.Types.Cpuid2
import qualified Ether.TagDispatch as I
import qualified Ether.State as I
import System.Linux.Kvm.Debug
import Control.Monad
import Control.Lens
import System.Linux.Kvm.IoCtl.Types.KvmGuestDebug
import System.Linux.Kvm.IoCtl.Types.PitConfig
import System.Linux.Kvm.Devices.Serial


import Args

vmSetup :: (MonadIO m, MonadRam m, MonadError m, MonadVm m) => m ()
vmSetup = do
              setTss 0xffffd000
              setIdentityMap 0xffffc000
              createPit2 $ PitConfig 0
              createIRQChip
              liftIO $ putStrLn "setup"
cpuSetup :: (MonadIO m, MonadCpu m, MonadError m) => m ()
cpuSetup = do
            
            -- I.tagAttach @Cpu $ guest_debug .= (guestDbgEnable <> guestDbgSinglestep)
            setCpuid $ Cpuid2 $ [mkCpuidEntry 0 1 0 0 0
                                ,mkCpuidEntry 1 0x400 0 0 0x701b179
                                ,mkCpuidEntry 0x80000000 0x80000001 0 0 0
                                ,mkCpuidEntry 0x80000001 0 0 0 0x20100800]
            return ()


vmHandle :: (MonadIO m, MonadVm m, MonadCpu m, MonadRam m, MonadHandlerState m) => KvmRunExit -> KvmRunT m ()
vmHandle = doHandle $ mconcat [genericSerialHandler 0x3f8
                              ,genericSerialHandler 0x2f8
                              ,genericSerialHandler 0x3e8
                              ,genericSerialHandler 0x2e8
                              ,handleHlt $ (liftIO $ putStrLn "\ngot hlt, stopping") >> stopVm
                              ,handleIOin'' 100 $ return 0 -- Not implemented
                              ,handleShutDown $ (liftIO $ putStrLn "[Shutdown]") >> stopVm
                              ,handleDebug $ (liftIO $ putStrLn "") >> return ()
                              ,defaultHandle $ \x -> return () -- \x-> liftIO $ putStr "Not handled KVM_EXIT" >> print x
                              ]

vmStop :: (MonadIO m) => m ()
vmStop = do
          return ()


main :: IO ()
main = do parseArgs $ do
                    x <- runError $ runKvmT $ runVmT $ runRam $ loadBzImage >> vmSetup >> (runCpuT $ initCpuRegs >> cpuSetup >> runStateHandlers (cpuContinueUntilVmEnd (\x -> vmHandle x ))) >> vmStop
                    either (\err -> liftIO $ putStrLn $ "error " ++ show err) (const $ return ()) x
