{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Linux.Kvm.ExitHandler where

import System.Linux.Kvm.IoCtl.Types.KvmRun
import Data.Word
import qualified Ether.State as I
import Data.TMap as T
import Data.Typeable
import Control.Applicative

data HandlerTag

data HandlerStates = HandlerStates { getTmap :: T.TMap }

type HandlerStateT m = I.StateT' HandlerStates m
type StatefulHandlerT a m = I.StateT HandlerTag a m
type StatefulHandler a m = ExitHandler (StatefulHandlerT a m)
type MonadHandlerState m = I.MonadState' HandlerStates m

data ExitHandler m = ExitHandler (KvmRunExit -> Maybe (m ()))

type MonadStatefulHandler a m = I.MonadState HandlerTag a m

instance Monad m => Semigroup (ExitHandler m) where
    (ExitHandler a) <> (ExitHandler b) = ExitHandler (\exit -> a exit <|> b exit)

instance Monad m => Monoid (ExitHandler m) where
    mempty = ExitHandler (const $ Nothing)

defaultHandle :: Monad m => (KvmRunExit -> m ()) -> ExitHandler m
defaultHandle handler = ExitHandler $ \x -> Just (handler x)

doHandle:: Monad m => ExitHandler m -> KvmRunExit -> m ()
doHandle (ExitHandler handler) exit = case (handler exit) of
                                        Nothing -> return ()
                                        Just x -> x

mkHandler:: Monad m => (KvmRunExit -> Bool) -> m () -> ExitHandler m
mkHandler test handler = ExitHandler $ \exit -> if (test exit) then Just handler else Nothing

handleHlt :: Monad m => m () -> ExitHandler m
handleHlt = mkHandler (==KvmRunExitHlt) 

handleShutDown :: Monad m => m () -> ExitHandler m
handleShutDown = mkHandler (==KvmRunExitShutdown) 

handleDebug:: Monad m => m () -> ExitHandler m
handleDebug = mkHandler (\x -> case x of 
                                (KvmRunExitDebug _) -> True
                                _ -> False)

makeStatefulHandler :: (Typeable a, MonadHandlerState m) => m a -> StatefulHandler a m -> ExitHandler m
makeStatefulHandler init (ExitHandler sh) = ExitHandler $ \runexit -> case (sh runexit) of
                                                                        Nothing -> Nothing
                                                                        Just handl -> Just $ do
                                                                                        (mv :: Maybe a) <- I.gets' (T.lookup . getTmap)
                                                                                        v <- (case mv of
                                                                                                Nothing -> init
                                                                                                Just v -> return v)
                                                                                        (a, s) <- I.runStateT @HandlerTag handl v
                                                                                        I.modify' $ \(HandlerStates x) -> HandlerStates $ T.insert s x

runStateHandlers:: (Monad m) => HandlerStateT m a -> m a
runStateHandlers act = I.evalStateT' act $ HandlerStates T.empty

getState :: MonadStatefulHandler a m => m a
getState = I.get @HandlerTag

setState :: MonadStatefulHandler a m => a -> m ()
setState = I.put @HandlerTag

modifyState :: MonadStatefulHandler a m => (a -> a) -> m ()
modifyState = I.modify @HandlerTag
