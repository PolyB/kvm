{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module System.Linux.Kvm.Devices.Serial where

#include <linux/serial_reg.h>

import System.Linux.Kvm.ExitHandler.Io
import System.Linux.Kvm.ExitHandler
import System.Linux.Kvm.Components.Ram
import Control.Monad.IO.Class
import Data.Word
import System.Linux.Kvm.KvmM.Cpu 
import System.Linux.Kvm.KvmM.Vm 
import qualified Data.ByteString as B
import Data.Array.IArray
import Data.Bits

data Serial = Serial    { 
                          thr :: Word8
                        , dll :: Word8
                        , dlm :: Word8
                        , iir :: Word8
                        , ier :: Word8
                        , fcr :: Word8
                        , lcr :: Word8
                        , mcr :: Word8
                        , lsr :: Word8
                        , scr :: Word8
                         }

data SerialCfg = SerialCfg { iobase :: Word16, irq :: Word8 }

type MonadSerial m = (MonadStatefulHandler Serial m, MonadIO m, MonadHandlerState m, MonadVm m)


getDlab :: MonadSerial m => m Bool
getDlab = ((/=0).(.&.(#const UART_LCR_DLAB)).lcr <$> getState)

caseDlab :: MonadSerial m => m a -> m a -> m a
caseDlab true false = getDlab >>= (\dlab -> if dlab then true else false)


getSerialChar :: MonadSerial m => m Word8
getSerialChar = return 0x41 -- A - TODO

putSerialChar :: MonadSerial m => Word8 -> m ()
putSerialChar c = do
                    liftIO $ B.putStr $ B.pack [c]
                    modifyState (\s -> s { iir = (iir s) .|. (#const UART_IIR_NO_INT) }) -- TODO : send IRQ

initialState :: Serial
initialState = Serial   { thr = 0
                        , dll = 0
                        , dlm = 0
                        , iir = (#const UART_IIR_NO_INT)
                        , ier = 0
                        , fcr = 0
                        , lcr = 0
                        , mcr = 0
                        , lsr = (#const UART_LSR_TEMT)  .|. (#const UART_LSR_THRE)
                        , scr = 0
                        }

genericSerialHandler :: (MonadIO m, MonadVm m, MonadRam m, MonadHandlerState m) => SerialCfg -> ExitHandler m
genericSerialHandler cfg@(SerialCfg base irq) = makeStatefulHandler
                                            (return initialState)
                                            (mconcat [ handleOut cfg
                                                     , handleIn cfg ])


handleOut:: MonadSerial m => SerialCfg -> ExitHandler m
handleOut (SerialCfg base irq) = handleIOgroupout (\port -> port >= base && port <= base + 7) $ (\h d -> getDlab >>= handle (h - base) d)
                                where   handle (#const UART_TX) dat False = putSerialChar dat >> irqLine (fromIntegral irq) 0
                                        handle (#const UART_IER) dat False = modifyState (\s -> s { ier = dat })
                                        handle (#const UART_FCR) dat False = modifyState (\s -> s { fcr = dat })
                                        handle (#const UART_LCR) dat False = modifyState (\s -> s { lcr = dat })
                                        handle (#const UART_MCR) dat False = modifyState (\s -> s { mcr = dat })
                                        handle (#const UART_SCR) dat False = modifyState (\s -> s { scr = dat })
                                        handle (#const UART_DLL) dat True = modifyState (\s -> s { dll = dat })
                                        handle (#const UART_DLM) dat True = modifyState (\s -> s { dlm = dat })
                                        handle (#const UART_FCR) dat True = modifyState (\s -> s { fcr = dat })
                                        handle (#const UART_LCR) dat True = modifyState (\s -> s { lcr = dat })
                                        handle iport idata dlab = return ()
                                                            


handleIn :: MonadSerial m => SerialCfg -> ExitHandler m
handleIn (SerialCfg base irq) = handleIOgroupin (\port -> port >= base && port <= base + 7) $ (\h -> getDlab >>= handle (h - base))
                                where   handle (#const UART_RX) False = irqLine (fromIntegral irq) 0 >> return 0xAA -- TODO getSerialChar
                                        handle (#const UART_IER) False = (ier <$> getState)
                                        -- handle (#const UART_IIR) False = (iir <$> getState)
                                        handle (#const UART_LCR) False = (lcr <$> getState)
                                        handle (#const UART_MCR) False = (mcr <$> getState)
                                        handle (#const UART_LSR) False = ((.|.(#const UART_LSR_DR)).lsr <$> getState)
                                        handle (#const UART_MSR) False = return (#const UART_MSR_CTS)
                                        handle (#const UART_SCR) False = (scr <$> getState)
                                        handle _ _ = return 0
