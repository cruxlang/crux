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
import Crux.Pos (Pos(..))

data Warning = Warning -- TODO: move this into Crux.Warning and give it real options

data TCState = TCState
    { tcWarnings :: IORef [Warning]
    , tcErrors   :: IORef [Error]
    , tcFileName :: FilePath
    }

newtype TC a = TC { unTC :: ReaderT TCState IO a }
    deriving (Functor, Applicative, MonadIO)

instance Monad TC where
    (TC m) >>= cb = TC $ m >>= unTC . cb
    return = TC . return

instance MonadFail TC where
    fail str = do
        TCState{tcFileName} <- TC ask
        failICE (InternalErrorPos tcFileName) $ InternalError str

data StopExecution = StopExecution Error
    deriving (Show)
instance Exception StopExecution

recordError :: Pos -> ErrorType -> TC ()
recordError pos e = do
    state <- TC ask
    modifyIORef' (tcErrors state) (Error pos e :)

failError :: Pos -> ErrorType -> TC a
failError pos e = do
    liftIO $ throwIO $ StopExecution $ Error pos e

failTypeError :: Pos -> TypeError -> TC a
failTypeError pos te = do
    failError pos $ TypeError te

failICE :: Pos -> InternalCompilerError -> TC a
failICE pos e = do
    failError pos $ InternalCompilerError e

recordWarning :: Warning -> TC ()
recordWarning w = do
    state <- TC ask
    modifyIORef' (tcWarnings state) (w:)

data TCResult a
    = TCSuccess a [Warning]
    | TCFail [Error] [Warning]

runTC :: FilePath -> TC a -> IO (TCResult a)
runTC tcFileName (TC m) = do
    tcWarnings <- newIORef []
    tcErrors <- newIORef []
    result <- try $ runReaderT m TCState{..}
    warnings <- reverse <$> readIORef tcWarnings
    errors <- reverse <$> readIORef tcErrors
    case result of
        Left (StopExecution lastError) -> do
            return $ TCFail (errors ++ [lastError]) warnings
        Right r -> do
            if errors == [] then do
                return $ TCSuccess r warnings
            else do
                return $ TCFail errors warnings

-- temporary -- used to convert existing code
bridgeTC :: FilePath -> TC a -> IO (Either Error a)
bridgeTC tcFileName (TC m) = do
    tcWarnings <- newIORef []
    tcErrors <- newIORef []
    result <- try $ runReaderT m TCState{..}
    errors <- reverse <$> readIORef tcErrors
    case errors of
        [] -> case result of
            Left (StopExecution lastError) ->
                return $ Left $ lastError
            Right r ->
                return $ Right r
        (firstError:_) -> do
            return $ Left firstError

-- temporary -- used to convert existing code
bridgeEitherTC :: FilePath -> TC a -> ExceptT Error IO a
bridgeEitherTC fileName m = ExceptT $ bridgeTC fileName m
