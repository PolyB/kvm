{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module System.Linux.Kvm.Errors
(
     Error(..)
    ,ErrorT
    ,MonadError
    ,runError
    ,execIO
    ,exTag
    ,throwE
) where

import qualified Ether.Except as I
import Control.Exception
import Control.Monad.IO.Class

data Error = ErrorIO IOException
           | ErrorTooMuchRamUsed
           | ErrorBadKernelFile
           | ErrorKernelTooOld
           | ErrorBadIoInHandler
           | ErrorTooManyE820Entries
           | ErrorBadAddress
           deriving (Show)

data ErrorTrace = ErrorTrace Error [String]
    deriving (Show)


type ErrorT = I.ExceptT' ErrorTrace

type MonadError m = I.MonadExcept' ErrorTrace m

-- | like `liftIO`, but `catch` the error into a `ErrorIO` `MonadError`
execIO:: (MonadIO m, MonadError m) => IO a -> m a
execIO act = do x <- liftIO $ catch (Right <$> act) (\ex -> return $ Left ex)
                either
                    (\ex -> I.throw' $ ErrorTrace (ErrorIO ex) ["execIO"])
                    return
                    x

runError:: Monad m => ErrorT m a -> m (Either ErrorTrace a)
runError = I.runExceptT'

exTag :: MonadError m => String -> m a -> m a
exTag tag act = let rethrow (ErrorTrace err trace) = I.throw' $ ErrorTrace err (tag:trace)
                in I.catch' act rethrow

throwE :: MonadError m => Error -> String -> m a
throwE err tag = I.throw' $ ErrorTrace err [tag]
