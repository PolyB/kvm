module System.Linux.Kvm.IoCtl.Types.InterruptMask where

import Foreign.Storable
import Data.Ix hiding (index)
import Data.Array.IArray hiding (index)
import Foreign.C.Types
import Foreign
import Foreign.Ptr
import Foreign.C
import Control.Lens

import Debug.Trace

#include <linux/kvm.h>

#define hsc_member_size(struct, field) \
  hsc_printf("%lu", (unsigned long) sizeof(((struct *)0)->field))

data InterruptMask = InterruptMask (Array Int Bool)
  deriving (Eq, Show)

nrInterrupts = (#const KVM_NR_INTERRUPTS)
interruptBitmapLength = (nrInterrupts + 63) `div` 64

setInterruptBit:: Bits a => Int -> [a] -> [a]
setInterruptBit pos arr = arr & traversed.index wordn %~ (`setBit`bitn)
                          where (wordn, bitn) = pos `divMod` 64

repack :: InterruptMask -> [Word64]
repack (InterruptMask arr) = foldl bitset base $ assocs arr
                              where base = replicate interruptBitmapLength zeroBits
                                    bitset arr (index, True) = setInterruptBit index arr
                                    bitset arr (_, False) = arr
unpack:: [Word64] -> InterruptMask
unpack words = InterruptMask $ array (0, nrInterrupts - 1)
                                              [(i, get i) | i <- [0 .. nrInterrupts - 1]]
                where get i = let (wordn, bitn) = i `divMod` 64
                              in testBit (words!!wordn) bitn

emptyInterruptMask:: InterruptMask
emptyInterruptMask = InterruptMask $ array (0, nrInterrupts - 1)
                                              [(i, False) | i <- [0 .. nrInterrupts - 1]]

instance Storable InterruptMask where
  sizeOf _ = (#member_size struct kvm_sregs, interrupt_bitmap)
  alignment _ = (#alignment __u64)
  peek ptr = do
               words <- (peekArray interruptBitmapLength (castPtr ptr::Ptr Word64)) :: IO [Word64]
               return $ unpack words
  poke ptr mask = pokeArray (castPtr ptr) (repack mask)
