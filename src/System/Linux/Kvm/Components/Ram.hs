{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module System.Linux.Kvm.Components.Ram (Ram, RamT, ConfigRamT, runConfigRam, runRam, MonadRam, mapMemRegion, MonadConfigRam, flatToHost, flatToHost', translateToHost, maxRam) where

import Control.Lens
import Control.Monad.IO.Class
import Data.Maybe
import Data.Word
import Foreign.Ptr
import System.Linux.Kvm.Errors
import System.Linux.Kvm.IoCtl.Types.UserspaceMemoryRegion
import System.Linux.Kvm.KvmM.Cpu 
import System.Linux.Kvm.KvmM.Vm
import Control.Monad
import qualified Ether.Reader as I
import qualified Ether.State as I
import qualified Ether.TagDispatch as I
import qualified System.Linux.Kvm.IoCtl as C

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
                    let defaultRam = 1 * 1024 * 1024 * 1024
                    I.runReaderT' m $ ConfigRam { maxRam=fromMaybe defaultRam val }



runRam::(MonadError m, MonadConfigRam m, MonadIO m, MonadVm m) => RamT m a -> m a
runRam act = do
                max <- I.asks' maxRam
                ptr <- execIO $ C.mmap max

                r <- I.evalStateT' (mapMemRegion ptr max 0>> act) $ Ram { _ramStart=ptr, _userSpaceMemRegions=[] }
                _ <- execIO $ C.c_munmap ptr (fromIntegral max) -- TODO : check return value
                return r

-- TODO : use userSpaceMemRegions for translating addresses
flatToHost::(MonadRam m) => Word64 -> m (Maybe (Ptr ()))
flatToHost addr = let getAddr (UserspaceMemoryRegion _ _ guestAddr regionSize userAddr) = if (addr >= guestAddr && addr < guestAddr + regionSize) then (Just $ plusPtr userAddr (fromIntegral addr)) else Nothing
                  in msum <$> map getAddr <$> I.gets' _userSpaceMemRegions

flatToHost'::(MonadRam m, MonadError m) => Word64 -> m (Ptr ())
flatToHost' addr = flatToHost addr >>= maybe (throwE ErrorBadAddress "flatToHost'") return

mapMemRegion:: (MonadError m, MonadRam m, MonadVm m, MonadIO m) => Ptr () -> Int -> Word64 -> m ()
mapMemRegion host size guest = do
                                    max_slot <- I.tagAttach @Ram $ use $ userSpaceMemRegions.to (map _slot).to (\x -> if null x then 0 else 1 + (maximum x))
                                    let newslot = UserspaceMemoryRegion { _slot = max_slot, _flags = 0, _guest_phys_addr = guest, _memory_size = (fromIntegral size), _userspace_addr = host }
                                    I.tagAttach @Ram $ userSpaceMemRegions %= (newslot:)
                                    fd <- vmfd
                                    execIO $ C.setUserMemoryRegion fd newslot

translateToHost :: (MonadRam m, MonadCpu m, MonadIO m) => Word64 -> m (Maybe (Ptr ()))
translateToHost addr = do
                         flat <- translateAddr addr
                         flatToHost flat
