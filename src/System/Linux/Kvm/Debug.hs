{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module System.Linux.Kvm.Debug (dumpRegs, dumpSRegs, dumpInstrs, dumpAtIp, dumpMem, dumpAll) where

import System.Linux.Kvm.KvmM.Cpu 
import Numeric
import System.Console.ANSI
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad
import System.Linux.Kvm.IoCtl.Types.Segment
import qualified Ether.TagDispatch as I
import Foreign.Marshal.Array
import Data.List
import Data.Bits
import Text.Disassembler.X86Disassembler
import Data.Word
import Foreign.Ptr
import System.Linux.Kvm.Components.Ram


debugHeader:: String -> IO ()
debugHeader header = do
                        setSGR [SetConsoleIntensity BoldIntensity ]
                        putStrLn $ "---[" ++ header ++ "]---"
                        setSGR [SetConsoleIntensity NormalIntensity ]

dumpText :: String -> [(String,String)] -> IO ()
dumpText n v = let
                    maxNameLen = maximum (length.fst <$> v)
                    maxValLen = maximum (length.snd <$> v)
                    maxlength = maxNameLen + maxValLen + 3
                    varPerLines = 120 `div` maxlength
                    align (var,val) = (var ++ replicate (maxNameLen - length var ) ' ', val ++ replicate (maxValLen - length val) ' ')
                    printvar (var, val) = var ++ " : " ++ setSGRCode [SetColor Foreground Vivid Blue] ++ val ++ setSGRCode []
                    printLine vars = putStrLn $ intercalate "  " (printvar <$> vars)
                    printVars x = printLine (take varPerLines x) >> (when (length x > varPerLines) $ printVars (drop varPerLines x))
                in do
                    debugHeader n
                    printVars (align <$> v)
            

dumpVars :: String -> (Integral a, Show a, FiniteBits a) => [(String, a)] -> IO ()
dumpVars n v = let  
                    maxNameLen = maximum (length.fst <$> v)
                    valLen = (finiteBitSize (snd$ v!!0)`div`8)
                    maxlength = maxNameLen + valLen + 6
                    varPerLines = 120 `div` maxlength
                    align (var,val) = (var ++ replicate (maxNameLen - length var ) ' ', replicate (valLen - length val) '0' ++ val)
                    printvar (var, val) = var ++ " : " ++ setSGRCode [SetColor Foreground Vivid Blue] ++ "0x" ++ val ++ setSGRCode []
                    printLine vars = putStrLn $ intercalate "  " (printvar <$> vars)
                    printVars x = printLine (take varPerLines x) >> (when (length x > varPerLines) $ printVars (drop varPerLines x))
                    toString (var, val) = (var, showHex val "")
                in do
                    debugHeader n
                    printVars (align.toString <$> v)
-- | Dump cpu regsisters
dumpRegs ::(MonadIO m, MonadCpu m) => m ()
dumpRegs = do
            r <- I.tagAttach @Cpu $ use regs
            liftIO $ dumpVars  "Regs" [("rax", r^.rax)
                                ,("rbx", r^.rbx)
                                ,("rcx", r^.rcx)
                                ,("rdx", r^.rdx)
                                ,("rsi", r^.rsi)
                                ,("rdi", r^.rdi)
                                ,("rsp", r^.rsp)
                                ,("rbp", r^.rbp)
                                ,("r8", r^.r8)
                                ,("r9", r^.r9)
                                ,("r10", r^.r10)
                                ,("r11", r^.r11)
                                ,("r12", r^.r12)
                                ,("r13", r^.r13)
                                ,("r14", r^.r14)
                                ,("r15", r^.r15)
                                ,("rip", r^.rip)
                                ,("rflags", r^.rflags) ]
-- | Dump cpu system registers
dumpSRegs :: (MonadIO m, MonadCpu m) => m ()
dumpSRegs = do
             let showSeg (Segment base limit selector stype present dpl db s l g avl) = "{base: 0x" ++ showHex base ", limit: 0x" ++ showHex limit ", selector : 0x" ++ showHex selector ", present :" ++ (if present /= 0 then "Y" else "N") ++ ", stype : 0x" ++ showHex stype "}"
             r <- I.tagAttach @Cpu $ use sregs
             liftIO $ dumpVars "System Regs" [("cr0", r^.cr0)
                               ,("cr2", r^.cr2)
                               ,("cr3", r^.cr3)
                               ,("cr4", r^.cr4)
                               ,("cr8", r^.cr8)]
             liftIO $ dumpText "Segments" [("cs", showSeg $ r^.cs)
                                          ,("ds", showSeg $ r^.ds)
                                          ,("es", showSeg $ r^.es)
                                          ,("fs", showSeg $ r^.fs)
                                          ,("gs", showSeg $ r^.gs)
                                          ,("ss", showSeg $ r^.ss)]

printInstrs :: [Instruction] -> IO ()
printInstrs instrs = forM_ instrs $ putStrLn.showAtt

-- | Dissasemble instructions at pointer
dumpInstrs :: Ptr Word8  -- ^ Pointer to the start of memory region
            -> Word64 -- ^ address of the pointer to the start of memory region (this will be printed in front of the instructions)
            -> Int  -- ^ Number of bytes to dissasemble
            -> IO ()
dumpInstrs ptr showAddr l = do 
                    r <- disassembleBlockWithConfig (defaultConfig { confStartAddr = fromIntegral showAddr }) ptr l
                    either (\_ -> dumpInstrs ptr showAddr (l - 1)) printInstrs r

-- | Dissasemble the next instructions from the cpu instruction pointer
dumpAtIp :: (MonadCpu m, MonadRam m, MonadIO m) => m ()
dumpAtIp = do
            liftIO $ debugHeader "Intruction dump at IP"
            regs <- I.tagAttach @Cpu $ use regs
            let ip = regs^.rip
            addr <- castPtr <$> translateToHost ip
            liftIO $ dumpInstrs addr ip 22


-- | Dump the Bytes of a memory region
dumpMem :: Ptr Word8 -- ^ Start of the memory region
        -> Int  -- ^ Size of the memory region
        -> IO ()
dumpMem ptr c = do  bytes <- peekArray c ptr
                    putStrLn $  foldr (\w s -> " 0x" ++ showHex w s) ""  bytes



-- | Dump the registers , system registers and next instructions
dumpAll :: (MonadIO m, MonadCpu m, MonadRam m) => m ()
dumpAll = dumpRegs >> dumpSRegs >> dumpAtIp
