{-# LANGUAGE TemplateHaskell #-}
module System.Linux.Kvm.KvmM.Utils where

import Foreign.Ptr
import Control.Lens
import System.Linux.Kvm.KvmM.Access
import Control.Monad.State.Strict

data LazyRef a = LazyRef { _dirty :: Bool ,_content :: Ptr a }
makeLenses ''LazyRef

-- lazyRefs:: MonadState s m => Lens s s (Maybe (LazyRef a)) (Maybe (LazyRef a)) -> m (Ptr a) -> m (PtrR a a)
-- lazyRefs lens ini = let
--                         doGet (Just (LazyRef _ content)) = return content
--                         doGet Nothing = do
--                                           iniptr <- ini
--                                           lens .= Just (LazyRef False iniptr)
--                                           return iniptr
--                       in PtrR<$> (use lens >>= doGet)
