{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds  #-}

module System.Linux.Kvm.Components.Console (Console, ConsoleT, MonadConsole, runConsole, readbyte, writebyte) where

import Control.Monad.IO.Class
import Data.Word
import System.IO
import qualified Ether.Reader as I
import qualified Data.ByteString as S
import Control.Monad

data Console = Console
    { input::Handle
    , output :: Handle
    , mustClose :: Bool }

type ConsoleT m = I.ReaderT' Console m

type MonadConsole m = I.MonadReader' Console m


setupConsole :: (MonadIO m) => (Handle,Handle) -> Bool -> m Console
setupConsole (inp,out) close = do
                                liftIO $ hSetEncoding inp char8
                                liftIO $ hSetEncoding out char8
                                liftIO $ hSetBuffering inp NoBuffering
                                liftIO $ hSetBuffering out NoBuffering
                                return $ Console { input = inp, output = out, mustClose=close }

initConsole:: (MonadIO m) => Maybe String -> m Console
initConsole Nothing = setupConsole (stdin, stdout) False
initConsole (Just path) = do
                        handle <- liftIO $ openBinaryFile path ReadWriteMode
                        setupConsole (handle, handle) True


readbyte:: (MonadIO m, MonadConsole m) => m Word8
readbyte = do
            o <- I.asks' input
            liftIO $ S.head <$> S.hGet o 1

writebyte:: (MonadIO m, MonadConsole m) => Word8 -> m ()
writebyte c = do 
                o <- I.asks' output
                liftIO $ S.hPut o (S.singleton c)

runConsole:: MonadIO m => Maybe String -> ConsoleT m a -> m a
runConsole path m = do
                     st@(Console inp out doclose)<- initConsole path
                     r <- I.runReaderT' m st
                     liftIO $ when doclose $ hClose inp >> hClose out
                     return r
