{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Foreign.Marshal.Array
import System.Linux.Kvm
import System.Linux.Kvm.IoCtl.Types.Segment
import System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunExit.Io
import System.Linux.Kvm.Components.Ram
import System.Linux.Kvm.Components.Init
import Data.Bits
import Data.Array.IArray
import qualified Ether.TagDispatch as I
import qualified Data.ByteString as B

import Args

-- vmSetup :: (MonadIO m, MonadRam m, MonadError m, MonadVm m) => m ()
-- vmSetup = do
--               setTss 0xffffd000
--               setIdentityMap 0xffffc000
--               ptr <- createMemRegion 4096 0x100000
--               liftIO $ pokeArray ptr [0xba, 0xf8, 0x03 -- mov 3f8 -> dx
--                                      ,0x00, 0xb8 -- add bl, al
--                                      ,0xb0, 0x41 -- mov 0xAA, al
--                                      ,0xee -- out al (dx)
--                                      ,0xee -- out al (dx)
--                                      ,0xf4 -- hlt
--                                      ]
--               liftIO $ putStrLn "setup"
            

vmStartup :: (MonadIO m, MonadError m, MonadCpu m) => m ()
vmStartup = do
              let setseg seg = (seg.base .= 0) >> (seg.limit .= 0xffffffff) >> (seg.g .= 1)
              I.tagAttach @Cpu $ do setseg $ sregs.cs
                                    sregs.cs.db .= 1
                                    setseg $ sregs.ds
                                    setseg $ sregs.ss
                                    sregs.ss.db .= 1

                                    sregs.cr0 %= (.|.1)

                                    regs.rflags .= 2
                                    regs.rip .= 0x100000

              liftIO $ putStrLn "startup"

vmHandle :: (MonadIO m, MonadVm m, MonadCpu m) => KvmRunExit -> KvmRunT m ()
vmHandle (KvmRunExitIo io) = case (io^.port, io^.direction) of 
                                (1016, IoDirectionOut) -> liftIO $ B.putStr $ B.pack (elems (io^.iodata))
                                _ -> liftIO $ putStrLn "other" >> print io
vmHandle a = do
                liftIO $ putStrLn "got unhandled exit :" >> print a

                stopVm

vmStop :: (MonadIO m) => m ()
vmStop = do
          liftIO $ putStrLn "stop"

main :: IO ()
main = do parseArgs $ do
                    x <- runError $ runKvmT $ runVmT $ runRam $ loadBzImage >>  (runCpuT $ vmStartup >> cpuContinueUntilVmEnd vmHandle) >> vmStop
                    either (\err -> liftIO $ putStrLn $ "error " ++ show err) (const $ return ()) x
          putStrLn "test"
