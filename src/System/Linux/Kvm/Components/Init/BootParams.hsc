{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module System.Linux.Kvm.Components.Init.BootParams where

import qualified Ether.Reader as I
import Foreign.Ptr

#include <asm/bootparam.h>

-- We define BootParams for the type, not for his content (thus no constructor)
data BootParams

type BootParamsT m = I.ReaderT' (Ptr BootParams) m

type MonadBootParams m = I.MonadReader' (Ptr BootParams) m

withBootParams:: Ptr BootParams -> BootParamsT m a -> m a
withBootParams = flip I.runReaderT'

bootParams:: MonadBootParams m => m (Ptr BootParams)
bootParams = I.ask'
