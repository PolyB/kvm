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
              -- createIRQChip
              liftIO $ putStrLn "setup"
cpuSetup :: (MonadIO m, MonadCpu m, MonadError m) => m ()
cpuSetup = do
            --I.tagAttach @Cpu $ guest_debug .= (guestDbgEnable <> guestDbgUseSwBp)
            return ()

serialHandler :: MonadIO m => ExitHandler m
serialHandler = mconcat [ handleIOin'' 0x3fd $ return 0x20
                        --,handleIOin'' 0x3fb $ return 0x20
                        ]


vmHandle :: (MonadIO m, MonadVm m, MonadCpu m, MonadRam m) => KvmRunExit -> KvmRunT m ()
vmHandle = doHandle $ mconcat [serialHandler
                              ,defaultHandle $ \x-> liftIO $  print x
                              ]

-- vmHandle (KvmRunExitIo io) = case (io^.port, io^.direction) of 
--                                     (0x3f8, IoDirectionOut) -> liftIO $ B.putStr $ B.pack (elems (io^.iodata))
--                                     (a, IoDirectionOut) | a > 0x3f8 && a < 0x3f8 + 6 -> return ()
--                                     _ -> liftIO $ print io
-- vmHandle (KvmRunExitDebug _) = do
--                                 regs <- I.tagAttach @Cpu $ use regs
--                                 let ip = regs^.rip
--                                 addr <- castPtr <$> translateToHost ip
--                                 instr <- liftIO $ peek addr
--                                 when (instr == (0xcc :: Word8)) $ do
--                                                                     liftIO $ poke addr (0xfc::Word8)
--                                                                     I.tagAttach @Cpu $ guest_debug .= (guestDbgEnable <> guestDbgSinglestep)
-- 
-- 
-- vmHandle a = do
--                 liftIO $ putStrLn "got unhandled exit :" >> print a
--                 stopVm
-- 
vmStop :: (MonadIO m) => m ()
vmStop = do
          liftIO $ putStrLn "stop"


main :: IO ()
main = do parseArgs $ do
                    x <- runError $ runKvmT $ runVmT $ runRam $ loadBzImage >> vmSetup >> (runCpuT $ initCpuRegs >> cpuSetup >> dumpAll >> cpuContinueUntilVmEnd (\x -> vmHandle x )) >> vmStop
                    either (\err -> liftIO $ putStrLn $ "error " ++ show err) (const $ return ()) x
