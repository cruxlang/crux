module Crux.Typecheck.Monad where

import Crux.Prelude
import Crux.Error (Error)
import Control.Monad.Trans.Reader

data Warning -- TODO: move this into Crux.Warning

data TCState = TCState
    { tcWarnings :: IORef [Warning]
    , tcErrors :: IORef [Error]
    }

type TC = ReaderT TCState IO

recordError :: Error -> TC ()
recordError e = do
    state <- ask
    modifyIORef' (tcErrors state) (e:)

recordWarning :: Warning -> TC ()
recordWarning w = do
    state <- ask
    modifyIORef' (tcWarnings state) (w:)

data TCResult a
    = TCSuccess a [Warning]
    | TCFail [Error] [Warning]

runTC :: TC a -> IO (TCResult a)
runTC m = do
    tcWarnings <- newIORef []
    tcErrors <- newIORef []
    result <- runReaderT m TCState{..}
    warnings <- reverse <$> readIORef tcWarnings
    errors <- reverse <$> readIORef tcErrors
    if errors == [] then
        return $ TCSuccess result warnings
    else
        return $ TCFail errors warnings

-- temporary -- used to convert existing code
bridgeTC :: TC a -> IO (Either Error a)
bridgeTC m = do
    tcWarnings <- newIORef []
    tcErrors <- newIORef []
    result <- runReaderT m TCState{..}
    errors <- reverse <$> readIORef tcErrors
    return $ case errors of
        [] -> Right result
        (x:_) -> Left x
