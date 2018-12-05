{-# LANGUAGE TemplateHaskell #-}

module System.Linux.Kvm.IoCtl.Types.Segment where

import Foreign.C.Types
import Foreign
import Foreign.Ptr
import Foreign.C
import Control.Lens

#include <linux/kvm.h>

data Segment = Segment {
   _base :: Word64
  ,_limit :: Word32
  ,_selector :: Word16
  ,_stype :: Word8
  ,_present, _dpl, _db, _s, _l, _g, _avl :: Word8
}
  deriving (Eq, Show)
makeLenses ''Segment

emptySegment :: Segment
emptySegment = Segment 0 0 0 0 0 0 0 0 0 0 0

instance Storable Segment where
  sizeOf _ = (#size struct kvm_segment)
  alignment _ = (#alignment struct kvm_segment)
  peek ptr = Segment  <$> (#peek struct kvm_segment, base) ptr 
                      <*> (#peek struct kvm_segment, limit) ptr 
                      <*> (#peek struct kvm_segment, selector) ptr 
                      <*> (#peek struct kvm_segment, type) ptr 
                      <*> (#peek struct kvm_segment, present) ptr 
                      <*> (#peek struct kvm_segment, dpl) ptr 
                      <*> (#peek struct kvm_segment, db) ptr 
                      <*> (#peek struct kvm_segment, s) ptr 
                      <*> (#peek struct kvm_segment, l) ptr 
                      <*> (#peek struct kvm_segment, g) ptr 
                      <*> (#peek struct kvm_segment, avl) ptr 

  poke ptr seg =  (#poke struct kvm_segment, base) ptr (seg^.base)
               >> (#poke struct kvm_segment, limit) ptr (seg^.limit)
               >> (#poke struct kvm_segment, selector) ptr (seg^.selector)
               >> (#poke struct kvm_segment, type) ptr (seg^.stype)
               >> (#poke struct kvm_segment, present) ptr (seg^.present)
               >> (#poke struct kvm_segment, dpl) ptr (seg^.dpl)
               >> (#poke struct kvm_segment, db) ptr (seg^.db)
               >> (#poke struct kvm_segment, s) ptr (seg^.s)
               >> (#poke struct kvm_segment, l) ptr (seg^.l)
               >> (#poke struct kvm_segment, g) ptr (seg^.g)
               >> (#poke struct kvm_segment, avl) ptr (seg^.avl)
