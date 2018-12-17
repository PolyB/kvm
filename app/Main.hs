module Main where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Foreign.Marshal.Array
import System.Linux.Kvm
import System.Linux.Kvm.IoCtl.Types.Segment
import System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunExit.Io
import Data.Bits
import Data.Array.IArray
import qualified Data.ByteString as B

vmSetup :: VmM IO ()
vmSetup = do
              setTss 0xffffd000
              setIdentityMap 0xffffc000
              ptr <- createMemRegion 4096 0x100000
              liftIO $ pokeArray ptr [0xba, 0xf8, 0x03 -- mov 3f8 -> dx
                                     ,0x00, 0xb8 -- add bl, al
                                     ,0xb0, 0x41 -- mov 0xAA, al
                                     ,0xee -- out al (dx)
                                     ,0xee -- out al (dx)
                                     ,0xf4 -- hlt
                                     ]
              liftIO $ putStrLn "setup"
            

vmStartup :: CpuM IO ()
vmStartup = do
              let setseg seg = (seg.base .= 0) >> (seg.limit .= 0xffffffff) >> (seg.g .= 1)
              setseg $ sregs.cs
              sregs.cs.db .= 1
              setseg $ sregs.ds
              setseg $ sregs.ss
              sregs.ss.db .= 1

              sregs.cr0 %= (.|.1)

              regs.rflags .= 2
              regs.rip .= 0x100000

              liftIO $ putStrLn "startup"

vmHandle :: KvmRunExit -> KvmRunBase -> CpuM IO ()
vmHandle (KvmRunExitIo io) a = case (io^.port, io^.direction) of 
                                  (1016, IoDirectionOut) -> liftIO $ B.putStr $ B.pack (elems (io^.iodata))
                                  _ -> liftIO $ putStrLn "other" >> print io
vmHandle a b = do
                  liftIO $ putStrLn "got unhandled exit :" >> print b >> print a

                  rgs <- use regs
                  liftIO $ print rgs
                  lift $ stopVm

vmStop :: CpuM IO ()
vmStop = do
          liftIO $ putStrLn "stop"

main :: IO ()
main = do
        startVm $ VMInstance {
          setup = vmSetup
         ,startup = vmStartup
         ,handle = vmHandle
         ,stop = vmStop
        }


