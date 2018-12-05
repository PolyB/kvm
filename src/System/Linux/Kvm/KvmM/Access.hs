{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}

module System.Linux.Kvm.KvmM.Access where

import Foreign.Ptr
import Foreign.Storable
import Control.Monad.IO.Class

data PtrR a b = PtrR (Ptr b)

class Monad m => Accessor a x m where
  accPoke :: PtrR x a -> a -> m ()
  accPeek :: PtrR x a -> m a


type Access a b = forall x m. (m(PtrR x a) -> m(PtrR x b))

(<=) :: (Accessor a x m) => m (PtrR x a) -> a -> m ()
a <= v = a >>= \ptr -> (accPoke ptr v)

rget :: (Accessor a x m) => m (PtrR x a) -> m a
rget a = a >>= \ptr -> (accPeek ptr)

(<~) :: (Accessor a x m) => m (PtrR x a) -> (a -> a) -> m ()
a <~ tr = do
            ptr <- a
            v <- accPeek ptr
            accPoke ptr $ tr v

(.->) :: Access a b -> Access b c -> Access a c
a.->b = b.a
