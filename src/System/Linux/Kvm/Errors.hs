{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module System.Linux.Kvm.Errors
(
     Error(..)
    ,ErrorT
    ,MonadError
    ,runError
    ,execIO
) where

import qualified Ether.Except as I
import Control.Exception
import Control.Monad.IO.Class

data Error = ErrorIO IOException
           | ErrorTooMuchRamUsed
           | ErrorBadKernelFile
           | ErrorKernelTooOld
           deriving (Show)


type ErrorT = I.ExceptT' Error

type MonadError m = I.MonadExcept' Error m

-- | like `liftIO`, but `catch` the error into a `ErrorIO` `MonadError`
execIO:: (MonadIO m, MonadError m) => IO a -> m a
execIO act = do x <- liftIO $ catch (Right <$> act) (\ex -> return $ Left ex)
                either
                    (\ex -> I.throw' $ ErrorIO ex)
                    return
                    x

runError:: Monad m => ErrorT m a -> m (Either Error a)
runError = I.runExceptT'
