{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
module System.Linux.Kvm.Components.Init (Init, InitT, MonadInit, runInit, getKernelPath, getInitRdPath) where

import qualified Ether.State as I

data Init = Init
    { 
        kernel :: String
        ,initrd :: Maybe String
    }

type InitT m = I.StateT' Init m

type MonadInit m = I.MonadState' Init m

runInit:: Monad m => String -> Maybe String -> I.StateT' Init m a -> m a
runInit ker ini m = do
                     I.evalStateT' m $ Init { kernel=ker, initrd=ini }

getKernelPath:: MonadInit m => m String
getKernelPath = I.gets' kernel

getInitRdPath:: MonadInit m => m (Maybe String)
getInitRdPath = I.gets' initrd
