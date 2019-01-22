{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
module System.Linux.Kvm.Components.Ram (Ram, RamT, runRam, MonadRam, getCurrentRamUsage, getMaxRamUsage, mmap) where

import qualified Ether.State as I
import qualified Ether.Except as I
import qualified System.Linux.Kvm.IoCtl as C
import System.Linux.Kvm.Errors
import Control.Monad.IO.Class
import Foreign.Ptr
import Control.Monad

data Ram = Ram
    { current :: Int
    , maxRam :: Maybe Int
    }

type RamT m = I.StateT' Ram m

type MonadRam m = I.MonadState' Ram m

runRam:: Monad m => Maybe Int -> I.StateT' Ram m a -> m a
runRam val m = do
                     I.evalStateT' m $ Ram { current=0, maxRam=val }

getCurrentRamUsage:: MonadRam m => m Int
getCurrentRamUsage = I.gets' current

getMaxRamUsage:: MonadRam m => m (Maybe Int)
getMaxRamUsage = I.gets' maxRam

mmap :: (MonadRam m, MonadError m, MonadIO m) => Int -> m (Ptr ())
mmap size = do
                curr <- getCurrentRamUsage
                maxRamUsed <- maybe False (\max -> curr + size > max) <$> getMaxRamUsage
                when maxRamUsed $ I.throw' ErrorTooMuchRamUsed
                execIO $ C.mmap size
