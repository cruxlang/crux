module Crux.TrackIO where

import Crux.Prelude
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (UnicodeException)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Exception.Base (IOException)
import Control.Exception (try)
import qualified Data.ByteString as BS

data Tracker = Tracker
    { trackPath :: FilePath -> IO ()
    }

type TrackIO a = ReaderT Tracker IO a

data TrackedReadError
    = IOError IOException
    | Utf8DecodeError UnicodeException

runTrackIO :: Tracker -> TrackIO a -> IO a
runTrackIO tracker action = runReaderT action tracker

runTrackIO' :: TrackIO a -> IO a
runTrackIO' = runTrackIO $ Tracker (\_ -> return ())

addTrackedPath :: FilePath -> TrackIO ()
addTrackedPath path = do
    tracker <- ask
    liftIO $ trackPath tracker path

readTrackedFile :: FilePath -> TrackIO (Either TrackedReadError ByteString)
readTrackedFile path = do
    -- even if the file doesn't exist, we want to track that we
    -- tried to access it
    addTrackedPath path
    result <- liftIO $ try $ BS.readFile path
    case result of
        Left ioerr -> do
            return $ Left $ IOError ioerr
        Right bytes -> do
            return $ Right bytes

readTrackedTextFile :: FilePath -> TrackIO (Either TrackedReadError Text)
readTrackedTextFile path = runEitherT $ do
    bytes <- EitherT $ readTrackedFile path
    case TE.decodeUtf8' bytes of
        Left err -> left $ Utf8DecodeError err
        Right text -> return text
