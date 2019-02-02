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
import System.Linux.Kvm.Components.Init
import Data.Array.IArray
import qualified Data.ByteString as B

import Args

vmSetup :: (MonadIO m, MonadRam m, MonadError m, MonadVm m) => m ()
vmSetup = do
              setTss 0xffffd000
              setIdentityMap 0xffffc000
              createIRQChip
              liftIO $ putStrLn "setup"
cpuSetup :: (MonadIO m, MonadCpu m, MonadError m) => m ()
cpuSetup = do
            setDebug (guestDbgEnable <> guestDbgSinglestep)
            

vmHandle :: (MonadIO m, MonadVm m, MonadCpu m) => KvmRunExit -> KvmRunT m ()
vmHandle (KvmRunExitIo io) = case (io^.port, io^.direction) of 
                                    (1016, IoDirectionOut) -> liftIO $ B.putStr $ B.pack (elems (io^.iodata))
                                    _ -> liftIO $ print io
vmHandle (KvmRunExitDebug _) = liftIO $ putStrLn "Debug exit"
vmHandle a = do
                liftIO $ putStrLn "got unhandled exit :" >> print a
                stopVm

vmStop :: (MonadIO m) => m ()
vmStop = do
          liftIO $ putStrLn "stop"

sep = liftIO $ replicateM_ 2 $ putStrLn "------------------------------------"

main :: IO ()
main = do parseArgs $ do
                    x <- runError $ runKvmT $ runVmT $ runRam $ loadBzImage >> vmSetup >> (runCpuT $ initCpuRegs >> cpuSetup >> dumpAll >> cpuContinueUntilVmEnd (\x -> sep >> vmHandle x >> dumpAll)) >> vmStop
                    either (\err -> liftIO $ putStrLn $ "error " ++ show err) (const $ return ()) x
