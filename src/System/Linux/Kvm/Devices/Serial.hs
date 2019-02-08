{-# LANGUAGE FlexibleContexts #-}

module System.Linux.Kvm.Devices.Serial where


import System.Linux.Kvm.ExitHandler.Io
import System.Linux.Kvm.ExitHandler
import System.Linux.Kvm.Components.Ram
import Control.Monad.IO.Class
import Data.Word
import System.Linux.Kvm.KvmM.Cpu 
import qualified Data.ByteString as B
import Data.Array.IArray

data Serial = Serial { ier :: Word8 }



genericSerialHandler :: (MonadIO m, MonadCpu m, MonadRam m, MonadHandlerState m) => Word16 -> ExitHandler m
genericSerialHandler port = makeStatefulHandler
                                (return Serial {ier = 0})
                                (mconcat [ handleIOin'' (port + 5) $ return 0x20
                                        , handleIOin'' (port + 1) $ (ier <$> getState)
                                        , handleIOout'' (port + 1) $ \x -> modifyState (\s -> s { ier = x})
                                        , handleIOout'' (port + 1) $ const $ return ()
                                        , handleIOout port $ (\x -> (liftIO $ B.putStr $ B.pack (elems x)))
                                        ])
