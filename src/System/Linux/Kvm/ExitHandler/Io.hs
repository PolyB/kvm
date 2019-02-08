{-# LANGUAGE FlexibleContexts #-}
module System.Linux.Kvm.ExitHandler.Io where

import System.Linux.Kvm.ExitHandler
import System.Linux.Kvm.IoCtl.Types.KvmRun.KvmRunExit.Io
import System.Linux.Kvm.IoCtl.Types.KvmRun
import Data.Word
import Control.Lens
import System.Linux.Kvm.Errors
import Foreign.Marshal.Array
import Data.Array.IArray
import qualified Ether.Except as I
import System.Linux.Kvm.Errors
import Control.Monad.IO.Class
import Control.Monad
import Data.Traversable

handleIOout :: Monad m => Word16 -> (Array Int Word8 -> m ()) -> ExitHandler m
handleIOout ioport handle = ExitHandler $ \h -> case h of
                                                    (KvmRunExitIo (Io (IoDirectionOut out) port _)) | port == ioport -> handle out >> return True
                                                    _ -> return False

handleIOout' :: Monad m => (Word16 -> Bool) -> (Array Int Word8 -> m ()) -> ExitHandler m
handleIOout' ioportC handle = ExitHandler $ \h -> case h of
                                                    (KvmRunExitIo (Io (IoDirectionOut out) port _)) | ioportC port -> handle out >> return True
                                                    _ -> return False

handleIOin :: (Monad m, MonadError m, MonadIO m) => Word16 -> (Word32 -> m [Word8]) -> ExitHandler m
handleIOin ioport handle = ExitHandler $ \h -> case h of
                                                    (KvmRunExitIo (Io (IoDirectionIn ptr) port size)) | port == ioport -> handle size >>= (\arr -> if (fromIntegral $ length arr) /= size then throwE ErrorBadIoInHandler "handleIoin" else liftIO $ pokeArray ptr arr) >> return True
                                                    _ -> return False

handleIOin' :: (Monad m, MonadError m, MonadIO m) => (Word16 -> Bool) -> (Word32 -> m [Word8]) -> ExitHandler m
handleIOin' ioportC handle = ExitHandler $ \h -> case h of
                                                    (KvmRunExitIo (Io (IoDirectionIn ptr) port size)) | ioportC port -> handle size >>= (\arr -> if (fromIntegral $ length arr) /= size then throwE ErrorBadIoInHandler "handleIOin'" else liftIO $ pokeArray ptr arr) >> return True
                                                    _ -> return False


handleIOin'' :: (Monad m, MonadIO m) => Word16 -> m Word8 -> ExitHandler m
handleIOin'' ioport handle = ExitHandler $ \h -> case h of
                                                    (KvmRunExitIo (Io (IoDirectionIn ptr) port size)) | ioport == port -> replicateM (fromIntegral size) handle >>= (\arr -> liftIO $ pokeArray ptr arr >> return True)
                                                    _ -> return False

handleIOout'' :: (Monad m, MonadIO m) => Word16 -> (Word8 -> m ()) -> ExitHandler m
handleIOout'' ioport handle = handleIOout ioport (\s -> void $ forM s handle)
