{-# LANGUAGE ScopedTypeVariables #-}

module Crux.TrackIO where

import Crux.Prelude
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (UnicodeException)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Exception.Base (IOException)
import Control.Exception (try)
import qualified Data.ByteString as BS
import System.FSNotify
import qualified System.FilePath as FP
import Control.Concurrent.STM
import qualified Data.Set as Set
import Data.List (sort)
import System.Directory (makeAbsolute)

data Tracker = Tracker
    { trackPath :: FilePath -> IO ()
    }

type TrackIO a = ReaderT Tracker IO a

data TrackedReadError
    = IOError IOException
    | Utf8DecodeError UnicodeException

runUntrackedIO :: TrackIO a -> IO a
runUntrackedIO = runTrackIO' $ Tracker $ \_ -> return ()

readAll :: TChan a -> IO [a]
readAll chan = do
    first <- atomically $ readTChan chan
    let loop sofar = do
            (atomically $ tryReadTChan chan) >>= \case
                Nothing -> return sofar
                Just e -> loop $ sofar ++ [e]
    loop [first]

loopWithTrackedIO :: TrackIO a -> IO a
loopWithTrackedIO action = do
    watchedPaths <- newIORef mempty

    changeQueue <- newTChanIO

    manager <- startManager
    let processEvent event = do
            let path = eventPath event
            -- putStrLn $ "processing event for: " ++ path
            wp <- readIORef watchedPaths
            when (Set.member path wp) $ do
                atomically $ writeTChan changeQueue path
    let tracker filePath' = do
            filePath <- makeAbsolute filePath'
            -- putStrLn $ "watching: " ++ filePath
            modifyIORef watchedPaths $ Set.insert filePath
            let loop dirPath = do
                    result <- try $ do
                        _ <- watchTree manager dirPath (const True) processEvent
                        return ()
                    case result of
                        Left (_err :: IOException) -> do
                            loop $ FP.takeDirectory dirPath
                        Right _ -> do
                            return ()
            loop $ FP.takeDirectory filePath

    let loop = do
            -- TODO: how should we unregister paths we no longer care about?
            writeIORef watchedPaths mempty
            _ <- runTrackIO' (Tracker tracker) action

            putStrLn "\nWaiting for changes..."

            changedPaths <- readAll changeQueue
            for_ (sort $ (Set.toList . Set.fromList) changedPaths) $ \path -> do
                putStrLn $ "  " ++ path
            putStrLn ""

            loop
    loop

runTrackIO' :: Tracker -> TrackIO a -> IO a
runTrackIO' tracker action = runReaderT action tracker

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
