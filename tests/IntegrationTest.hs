{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE QuasiQuotes #-}

module IntegrationTest where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.MVar
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMQueue
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (replicateM, when)
import Crux.Error (TypeError (..))
import qualified Crux.Error as Error
import qualified Crux.Gen as Gen
import qualified Crux.JSBackend as JS
import qualified Crux.Module
import Crux.Module.Types as AST
import Crux.ModuleName
import Crux.Pos (Pos(..), PosRec(..))
import Crux.TypeVar (renderTypeVarIO)
import Data.Foldable (for_)
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Yaml as Yaml
import System.Directory (doesFileExist)
import System.Directory (doesDirectoryExist)
import qualified System.Directory.PathWalk as PathWalk
import System.Exit (ExitCode (..))
import qualified System.FilePath as FilePath
import System.Process (readProcessWithExitCode)
import Test.Framework
import Text.RawString.QQ (r)

runProgram' :: AST.Program -> IO String
runProgram' p = do
    m' <- Gen.generateProgram p
    rtsSource <- Crux.Module.loadRTSSource
    let js = JS.generateJS rtsSource m'
    readProcessWithExitCode "node" [] (T.unpack js) >>= \case
        (ExitSuccess, stdoutBody, _) -> do
            return stdoutBody
        (ExitFailure code, _, stderr) -> do
            putStrLn $ "Process failed with code: " ++ show code ++ "\n" ++ stderr
            putStrLn "Code:"
            T.putStrLn js
            fail $ "Process failed with code: " ++ show code ++ "\n" ++ stderr

makePos :: Int -> Int -> Pos
makePos l c = Pos $ PosRec
    { posFileName = "<main>"
    , posLine = l
    , posColumn = c
    }

run :: Text -> IO (Either Error.Error String)
run src = do
    Crux.Module.loadProgramFromSource src >>= \case
        Left e -> return $ Left e
        Right m -> fmap Right $ runProgram' m

runMultiModule :: HashMap.HashMap ModuleName Text -> IO (Either Error.Error String)
runMultiModule sources = do
    Crux.Module.loadProgramFromSources sources >>= \case
        Left e -> return $ Left e
        Right m -> fmap Right $ runProgram' m

assertCompiles src = do
    result <- run $ T.unlines src
    case result of
        Right _ -> return ()
        Left err -> assertFailure $ "Compile failure: " ++ show err

assertOutput src outp = do
    result <- run $ T.unlines src
    case result of
        Right a -> assertEqual outp a
        Left err -> assertFailure $ "Compile failure: " ++ show err

assertUnificationError :: Pos -> String -> String -> Either Error.Error a -> IO ()
assertUnificationError pos a b (Left (Error.Error actualPos (Error.TypeError (UnificationError _ at bt)))) = do
    assertEqual pos actualPos

    as <- renderTypeVarIO at
    bs <- renderTypeVarIO bt
    assertEqual a as
    assertEqual b bs
assertUnificationError _ _ _ (Left err) =
    assertFailure $ "Expected a unification error, got: " ++ show err
assertUnificationError _ _ _ _ =
    assertFailure "Expected a unification error"

failWithError :: String -> Error.Error -> IO ()
failWithError root err = do
    err' <- Error.renderError err
    assertFailure $ "\nError in: " <> root <> "\n" <> err'

data ErrorConfig = ErrorConfig
    { errorName     :: Maybe Text
    , typeErrorName :: Maybe Text
    }

instance Yaml.FromJSON ErrorConfig where
    parseJSON (Yaml.Object o) = do
        ErrorConfig <$> o Yaml..:? "error-name" <*> o Yaml..:? "type-error-name"
    parseJSON _ = fail "error config must be an object"

assertErrorMatches :: ErrorConfig -> Error.Error -> IO ()
assertErrorMatches ErrorConfig{..} theErr@(Error.Error _ err) = do
    case errorName of
        Just errorName' -> do
            assertEqual errorName' $ Error.getErrorName err
        Nothing -> do
            return ()

    case (typeErrorName, err) of
        (Just typeErrorName', Error.TypeError te) -> do
            assertEqual typeErrorName' $ Error.getTypeErrorName te
        (Just _, _) -> do
            msg <- Error.renderError theErr
            assertFailure $ "type-error-name specified, but not a type error!\n" ++ msg
        (Nothing, _) -> do
            return ()

runIntegrationTest :: FilePath -> IO (IO ())
runIntegrationTest root = do
    let mainPath = FilePath.combine root "main.cx"
    let stdoutPath = FilePath.combine root "stdout.txt"
    let errorPath = FilePath.combine root "error.yaml"

    let intro = "testing program " ++ mainPath

    stdoutExists <- doesFileExist stdoutPath
    errorExists <- doesFileExist errorPath

    --traceMarkerIO "loading program"
    program' <- Crux.Module.loadProgramFromFile mainPath
    --traceMarkerIO "loaded program"

    (putStrLn intro >>) <$> if stdoutExists then do
        expected <- readFile stdoutPath

        case program' of
            Left err -> do
                return $ do
                    failWithError root err
            Right program -> do
                --traceMarkerIO "running program"
                stdoutBody <- runProgram' program
                --traceMarkerIO "ran program"
                return $ do
                    assertEqual expected stdoutBody
    else if errorExists then do
        errorConfig' <- Yaml.decodeFileEither errorPath
        case (errorConfig', program') of
            (Left err, _) -> return $ assertFailure $ show err
            (_, Right _) -> return $ assertFailure "Program should have produced a compile error"
            (Right errorConfig, Left err) -> return $ assertErrorMatches errorConfig err
    else do
        return $ do
            assertFailure "Program needs either a stdout.txt or error.yaml"

test_integration_tests = do
    let integrationRoot = "tests/integration"
    exists <- doesDirectoryExist integrationRoot
    when (not exists) $ do
        fail $ "Integration test directory " ++ integrationRoot ++ " does not exist!"

    -- assume each test is cpu-bound
    capCount <- getNumCapabilities

    putStrLn $ "Running tests with " ++ show capCount ++ " threads"

    testQueue <- newTMQueueIO
    resultQueue <- newTMQueueIO

    testProducer <- async $ do
        PathWalk.pathWalk integrationRoot $ \d _dirnames filenames -> do
            when ("main.cx" `elem` filenames) $ do
                result <- newEmptyMVar
                atomically $ do
                    writeTMQueue testQueue $ do
                        try (runIntegrationTest d) >>= putMVar result
                    writeTMQueue resultQueue $ result
        atomically $ do
            closeTMQueue testQueue
            closeTMQueue resultQueue

    let runTests = do
            atomically (readTMQueue testQueue) >>= \case
                Just t -> t >> runTests
                Nothing -> return ()
    runners <- replicateM capCount $ async $ runTests

    let consumeResults = do
            atomically (readTMQueue resultQueue) >>= \case
                Just mvar -> do
                    readMVar mvar >>= \case
                        Left exc -> throwIO (exc :: SomeException)
                        Right action -> action
                    consumeResults
                Nothing -> do
                    return ()
    consumeResults

    wait testProducer
    for_ runners wait

test_annotation_is_checked = do
    result <- run $ T.unlines
        [ "let i: Number = \"hody\""
        ]

    assertUnificationError (makePos 1 1) "Number" "String" result

test_arrays_of_different_types_cannot_unify = do
    result <- run $ T.unlines
        [ "let _ = [[0], [\"\"]]"
        ]
    assertUnificationError (makePos 1 9) "Number" "String" result

test_polymorphic_type_annotations_are_universally_quantified2 = do
    rv <- run $ T.unlines
        [ "let f: fun(Number) -> Number = fun (i) { i }"
        , "let g<a>: fun(a) -> a = fun (i) { i }"
        , "let _ = f(g(\"hello\"))"
        ]
    assertUnificationError (makePos 3 11) "String" "Number" rv

test_escaped_strings = do
    result1 <- run $ T.unlines
        [ [r|let _ = print("\0\a\b\f\n\r\t\v\\\'\"\?")|]
        ]
    assertEqual (Right "\NUL\a\b\f\n\r\t\v\\'\"?\n") result1

    result2 <- run $ T.unlines
        [ [r|let _ = print("\x00\x01\x02")|]
        ]
    assertEqual (Right "\0\1\2\n") result2

    -- TODO: tests for \u and \U
