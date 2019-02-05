{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Lens
import Control.Monad.IO.Class
import System.Linux.Kvm
import System.Linux.Kvm.Debug
import System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunExit.Io
import System.Linux.Kvm.Components.Ram
import Control.Monad
import System.Linux.Kvm.IoCtl.Types.KvmGuestDebug
import System.Linux.Kvm.IoCtl.Types.Cpuid2
import System.Linux.Kvm.Components.Init
import Data.Array.IArray
import qualified Data.ByteString as B
import qualified Ether.TagDispatch as I
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import System.Linux.Kvm.ExitHandler
import System.Linux.Kvm.ExitHandler.Io

import Args

vmSetup :: (MonadIO m, MonadRam m, MonadError m, MonadVm m) => m ()
vmSetup = do
              setTss 0xffffd000
              setIdentityMap 0xffffc000
              createIRQChip
              liftIO $ putStrLn "setup"
cpuSetup :: (MonadIO m, MonadCpu m, MonadError m) => m ()
cpuSetup = do
            
            I.tagAttach @Cpu $ guest_debug .= (guestDbgEnable <> guestDbgSinglestep)
            setCpuid $ Cpuid2 $ [mkCpuidEntry 0 1 0 0 0
                                ,mkCpuidEntry 1 0x400 0 0 0x701b179
                                ,mkCpuidEntry 0x80000000 0x80000001 0 0 0
                                ,mkCpuidEntry 0x80000001 0 0 0 0x20100800]
            return ()

serialHandler :: (MonadIO m, MonadCpu m, MonadRam m)=> ExitHandler m
serialHandler = mconcat [ handleIOin'' 0x3fd $ return 0x20
                        --,handleIOin'' 0x3fb $ return 0x20
                        , handleIOout 0x3f8 $ (\x -> (liftIO $ B.putStr $ B.pack (elems x)))-- >> (when (x!0 == 0x42) $ I.tagAttach @Cpu $ guest_debug .= (guestDbgEnable <> guestDbgSinglestep)))
                        ]


vmHandle :: (MonadIO m, MonadVm m, MonadCpu m, MonadRam m) => KvmRunExit -> KvmRunT m ()
vmHandle = doHandle $ mconcat [serialHandler
                              ,handleHlt $ (liftIO $ putStrLn "\ngot hlt, stopping") >> stopVm
                              ,handleShutDown $ (liftIO $ putStrLn "[Shutdown]") >> stopVm
                              ,handleDebug $ dumpAll
                              ,defaultHandle $ \x-> liftIO $  print x
                              ]

vmStop :: (MonadIO m) => m ()
vmStop = do
          return ()


main :: IO ()
main = do parseArgs $ do
                    x <- runError $ runKvmT $ runVmT $ runRam $ loadBzImage >> vmSetup >> (runCpuT $ initCpuRegs >> cpuSetup >> dumpAll >> cpuContinueUntilVmEnd (\x -> vmHandle x )) >> vmStop
                    either (\err -> liftIO $ putStrLn $ "error " ++ show err) (const $ return ()) x
