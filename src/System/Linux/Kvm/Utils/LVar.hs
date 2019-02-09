{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module System.Linux.Kvm.Utils.LVar where

import Control.Monad

type LVar a = Maybe (Bool, a)

class Monad m => LazyCpuVar m a where
    lread :: m (LVar a)
    lwrite :: LVar a -> m ()
    vread :: m a
    vwrite :: a -> m ()


lget:: LazyCpuVar m a => m a
lget = do
        v <- lread
        case v of
            Just (_, x) -> return x
            Nothing -> do 
                        rv <- vread
                        lwrite $ Just (False, rv)
                        return rv

lset:: (LazyCpuVar m a) => a -> m ()
lset nv = lwrite $ Just (True, nv)

lmodify :: (LazyCpuVar m a) => (a -> a) -> m ()
lmodify mod = lget >>= lset.mod

lflush:: forall a m. LazyCpuVar m a => m ()
lflush = do
            (v ::LVar a) <- lread
            case v of
                Nothing -> return ()
                Just (modified, x) -> do
                                        when modified $ vwrite x
                                        lwrite (Nothing :: LVar a)
