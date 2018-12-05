{-# LANGUAGE TemplateHaskell #-}

module System.Linux.Kvm.IoCtl.Types.DTable where

import Foreign.C.Types
import Foreign
import Foreign.Ptr
import Foreign.C
import Control.Lens

#include <linux/kvm.h>

data DTable = DTable {
  _base :: Word64,
  _limit :: Word16
}
  deriving (Show, Eq)
makeLenses ''DTable

emptyDtable :: DTable
emptyDtable = DTable { _base = 0, _limit = 0 }

instance Storable DTable where
  sizeOf _ = (#size struct kvm_dtable)
  alignment _ = (#alignment struct kvm_dtable)
  peek ptr = DTable <$> (#peek struct kvm_dtable, base) ptr 
                    <*> (#peek struct kvm_dtable, limit) ptr 

  poke ptr dtable =  (#poke struct kvm_segment, base) ptr (dtable^.base)
                  >> (#poke struct kvm_segment, limit) ptr (dtable^.limit)
