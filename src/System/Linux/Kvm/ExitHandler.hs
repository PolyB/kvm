module System.Linux.Kvm.ExitHandler where

import System.Linux.Kvm.IoCtl.Types.KvmRun

data ExitHandler m = ExitHandler (KvmRunExit -> KvmRunT m Bool)

instance Monad m => Semigroup (ExitHandler m) where
    (ExitHandler a) <> (ExitHandler b) = ExitHandler (\exit -> a exit >>= \x -> if x then return x else b exit)

instance Monad m => Monoid (ExitHandler m) where
    mempty = ExitHandler (const $ return False)

defaultHandle :: Monad m => (KvmRunExit -> KvmRunT m ()) -> ExitHandler m
defaultHandle handler = ExitHandler $ \x -> (handler x >> return True)

doHandle:: Monad m => ExitHandler m -> KvmRunExit -> KvmRunT m ()
doHandle (ExitHandler handler) exit = handler exit >> return ()

mkHandler:: Monad m => (KvmRunExit -> Bool) -> KvmRunT m () -> ExitHandler m
mkHandler test handler = ExitHandler $ \exit -> if (test exit) then handler >> return True else return False

handleHlt :: Monad m => KvmRunT m () -> ExitHandler m
handleHlt = mkHandler (==KvmRunExitHlt) 

handleShutDown :: Monad m => KvmRunT m () -> ExitHandler m
handleShutDown = mkHandler (==KvmRunExitShutdown) 
