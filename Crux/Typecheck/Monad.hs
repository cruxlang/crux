{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Crux.Typecheck.Monad
    ( TC
    , Warning(..)
    , recordError
    , failError
    , failICE
    , failTypeError
    , recordWarning
    , runTC
    , bridgeTC
    , bridgeEitherTC
    ) where

import Control.Exception (Exception, throwIO, try)
import Control.Monad.Trans.Reader
import Crux.Error
import Crux.Prelude
import Crux.Tokens (Pos)

data Warning = Warning -- TODO: move this into Crux.Warning and give it real options

data TCState = TCState
    { tcWarnings :: IORef [Warning]
    , tcErrors   :: IORef [Error]
    }

newtype TC a = TC (ReaderT TCState IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

data StopExecution = StopExecution
    deriving (Show)
instance Exception StopExecution

stopChecking :: TC a
stopChecking = liftIO $ do
    throwIO StopExecution

recordError :: Error -> TC ()
recordError e = do
    state <- TC ask
    modifyIORef' (tcErrors state) (e:)

failError :: Error -> TC a
failError e = do
    recordError e
    stopChecking

failTypeError :: Pos -> TypeError -> TC a
failTypeError pos te = do
    recordError $ TypeError pos te
    stopChecking

failICE :: InternalCompilerError -> TC a
failICE e = do
    failError $ InternalCompilerError e

recordWarning :: Warning -> TC ()
recordWarning w = do
    state <- TC ask
    modifyIORef' (tcWarnings state) (w:)

data TCResult a
    = TCSuccess a [Warning]
    | TCFail [Error] [Warning]

runTC :: TC a -> IO (TCResult a)
runTC (TC m) = do
    tcWarnings <- newIORef []
    tcErrors <- newIORef []
    result <- try $ runReaderT m TCState{..}
    warnings <- reverse <$> readIORef tcWarnings
    errors <- reverse <$> readIORef tcErrors
    if errors == [] then
        case result of
            Left StopExecution ->
                return $ TCFail [InternalCompilerError StoppedCheckingWithNoError] warnings
            Right r ->
                return $ TCSuccess r warnings
    else
        return $ TCFail errors warnings

-- temporary -- used to convert existing code
bridgeTC :: TC a -> IO (Either Error a)
bridgeTC (TC m) = do
    tcWarnings <- newIORef []
    tcErrors <- newIORef []
    result <- try $ runReaderT m TCState{..}
    errors <- reverse <$> readIORef tcErrors
    case errors of
        [] -> case result of
            Left StopExecution ->
                return $ Left $ InternalCompilerError StoppedCheckingWithNoError
            Right r ->
                return $ Right r
        (firstError:_) -> do
            return $ Left firstError

-- temporary -- used to convert existing code
bridgeEitherTC :: TC a -> EitherT Error IO a
bridgeEitherTC m = EitherT $ bridgeTC m
