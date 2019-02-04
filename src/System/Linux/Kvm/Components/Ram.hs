{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module System.Linux.Kvm.Components.Ram (Ram, RamT, ConfigRamT, runConfigRam, runRam, MonadRam, flatToHost, realToHost, translateToHost) where

import qualified Ether.Reader as I
import qualified Ether.State as I
import qualified Ether.TagDispatch as I
import qualified System.Linux.Kvm.IoCtl as C
import System.Linux.Kvm.IoCtl.Types.UserspaceMemoryRegion
import System.Linux.Kvm.KvmM.Cpu 
import System.Linux.Kvm.KvmM.Vm
import System.Linux.Kvm.Errors
import Control.Monad.IO.Class
import Foreign.Ptr
import Data.Maybe
import Data.Word
import Control.Lens
import Data.Bits

data ConfigRam = ConfigRam { maxRam :: Int }
data Ram = Ram
            {
                _ramStart :: Ptr ()
               ,_userSpaceMemRegions :: [UserspaceMemoryRegion]
            }
makeLenses ''Ram

type ConfigRamT m = I.ReaderT' ConfigRam m
type RamT m = I.StateT' Ram m

type MonadRam m = I.MonadState' Ram m
type MonadConfigRam m = I.MonadReader' ConfigRam m

runConfigRam:: Monad m => Maybe Int -> ConfigRamT m a -> m a
runConfigRam val m = do
                    let defaultRam = 256 * 1024 * 1024-- 1 * 1024 * 1024 * 1024
                    I.runReaderT' m $ ConfigRam { maxRam=fromMaybe defaultRam val }



runRam::(MonadError m, MonadConfigRam m, MonadIO m, MonadVm m) => RamT m a -> m a
runRam act = do
                max <- I.asks' maxRam
                ptr <- execIO $ C.mmap max

                r <- I.evalStateT' (mapMemRegion ptr max 0>> act) $ Ram { _ramStart=ptr, _userSpaceMemRegions=[] }
                _ <- execIO $ C.c_munmap ptr (fromIntegral max) -- TODO : check return value
                return r

-- TODO : use userSpaceMemRegions for translating addresses
flatToHost::(MonadRam m) => Word64 -> m (Ptr ())
flatToHost off = (\x -> plusPtr x (fromIntegral off)) <$> I.gets' _ramStart


realToHost::(MonadRam m) => Word16 -> Word16 -> m (Ptr ())
realToHost sel off = let flat = ((fromIntegral sel) * 16) + (fromIntegral off)
                     in flatToHost flat

mapMemRegion:: (MonadError m, MonadRam m, MonadVm m, MonadIO m) => Ptr () -> Int -> Word64 -> m ()
mapMemRegion host size guest = do
                                    max_slot <- I.tagAttach @Ram $ use $ userSpaceMemRegions.to (map _slot).to (\x -> if null x then 0 else 1 + (maximum x))
                                    let newslot = UserspaceMemoryRegion { _slot = max_slot, _flags = 0, _guest_phys_addr = guest, _memory_size = (fromIntegral size), _userspace_addr = host }
                                    I.tagAttach @Ram $ userSpaceMemRegions %= (newslot:)
                                    fd <- vmfd
                                    execIO $ C.setUserMemoryRegion fd newslot

translateToHost :: (MonadRam m, MonadCpu m, MonadIO m) => Word64 -> m (Ptr ())
translateToHost addr = do
                         flat <- translateAddr addr
                         flatToHost flat
                    
