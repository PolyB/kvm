{-# LANGUAGE ForeignFunctionInterface #-}
module System.Linux.Kvm.IoCtl.Types.PitConfig where

import Foreign.Storable
import Data.Word

#include <linux/kvm.h>

data PitConfig = PitConfig
                    {
                    pitflags :: Word32
                    }
                    deriving (Show)

instance Storable PitConfig where
    sizeOf _ = (#size struct kvm_pit_config)
    alignment _ = (#alignment struct kvm_pit_config)
    peek ptr = PitConfig <$> (#peek struct kvm_pit_config, flags) ptr

    poke ptr cfg = (#poke struct kvm_pit_config, flags) ptr (pitflags cfg)
