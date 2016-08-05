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
import Crux.Pos (Pos (..))
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

run :: Text -> IO (Either Error.Error String)
run src = do
    Crux.Module.loadProgramFromSource src >>= \case
        Left (_, e) -> return $ Left e
        Right m -> fmap Right $ runProgram' m

runMultiModule :: HashMap.HashMap ModuleName Text -> IO (Either Error.Error String)
runMultiModule sources = do
    Crux.Module.loadProgramFromSources sources >>= \case
        Left (_, e) -> return $ Left e
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
assertUnificationError pos a b (Left (Error.TypeError actualPos (UnificationError _ at bt))) = do
    assertEqual pos actualPos

    as <- renderTypeVarIO at
    bs <- renderTypeVarIO bt
    assertEqual a as
    assertEqual b bs
assertUnificationError _ _ _ (Left err) =
    assertFailure $ "Expected a unification error, got: " ++ show err
assertUnificationError _ _ _ _ =
    assertFailure "Expected a unification error"

failWithError :: String -> Maybe ModuleName -> Error.Error -> IO ()
failWithError root moduleName err = do
    err' <- Error.renderError moduleName err
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
assertErrorMatches ErrorConfig{..} err = do
    case errorName of
        Just errorName' -> do
            assertEqual errorName' $ Error.getErrorName err
        Nothing -> do
            return ()

    case (typeErrorName, err) of
        (Just typeErrorName', Error.TypeError _pos te) -> do
            assertEqual typeErrorName' $ Error.getTypeErrorName te
        (Just _, _) -> do
            msg <- Error.renderError' err
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
            Left (moduleName, err) -> do
                return $ do
                    failWithError root moduleName err
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
            (Right errorConfig, Left (_moduleName, err)) -> return $ assertErrorMatches errorConfig err
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

    assertUnificationError (Pos 1 1 1) "Number" "String" result

test_arrays_of_different_types_cannot_unify = do
    result <- run $ T.unlines
        [ "let _ = [[0], [\"\"]]"
        ]
    assertUnificationError (Pos 1 1 9) "Number" "String" result

test_record_annotation_is_checked2 = do
    result <- run $ T.unlines
        [ "let c: {} = _unsafe_js(\"console\")"
        , "fun main() {"
        , "    c.log(\"Hoop\")"
        , "}"
        , "let _ = main()"
        ]

    assertUnificationError (Pos 5 3 5) "{}" "{log: (TUnbound fromList [] 10),..._11}" result
    -- assertEqual (Left "Unification error: Field 'log' not found in quantified record {} and {log: (TUnbound 6),f...}") result

test_cannot_assign_to_immutable_binding = do
    result <- run $ T.unlines
        [ "fun main() {"
        , "    let x = 2"
        , "    x = x + 1"
        , "    print(x)"
        , "}"
        , "let _ = main()"
        ]

    -- assertEqual (Left "Not an lvar: EIdentifier (IPrimitive Number) (Local \"x\")") result
    assertEqual (Left $ Error.TypeError (Pos 5 3 5) $ NotAnLVar "EIdentifier Pos 5 3 5 (UnqualifiedReference \"x\")") result

test_cannot_assign_to_immutable_record_field = do
    result <- run $ T.unlines
        [ "fun main() {"
        , "    let a: {const x: Number} = {x: 44}"
        , "    a.x = 22"
        , "    print(a)"
        , "}"
        , "let _ = main()"
        ]

    assertEqual
        -- (Left "Not an lvar: ELookup (IPrimitive Number) (EIdentifier (IRecord (RecordType RecordClose [TypeRow {trName = \"x\", trMut = RImmutable, trTyVar = IPrimitive Number}])) (Local \"a\")) \"x\"")
        (Left $ Error.TypeError (Pos 5 3 5) $ NotAnLVar "ELookup Pos 5 3 5 (EIdentifier Pos 5 3 5 (UnqualifiedReference \"a\")) \"x\"")
        result

test_mutable_record_field_requirement_is_inferred = do
    result <- run $ T.unlines
        [ "fun swap(p) {"
        , "    let t = p.x"
        , "    p.x = p.y"
        , "    p.y = t"
        , "}"
        , "fun main() {"
        , "    let a: {const x: Number, const y: Number} = {x:44, y:0}"
        , "    swap(a)"
        , "}"
        , "let _ = main()"
        ]

    assertEqual
        (Left $ Error.TypeError (Pos 5 8 10) $ RecordMutabilityUnificationError "x" "Record field mutability does not match")
        result

test_polymorphic_type_annotations_are_universally_quantified2 = do
    rv <- run $ T.unlines
        [ "let f: fun(Number) -> Number = fun (i) { i }"
        , "let g<a>: fun(a) -> a = fun (i) { i }"
        , "let _ = f(g(\"hello\"))"
        ]
    assertUnificationError (Pos 1 3 11) "String" "Number" rv

test_polymorphic_type_annotations_are_universally_quantified4 = do
    rv <- run $ T.unlines
        [ "let f<a>: fun(a) -> Number = fun (i) { i }"
        ]
    assertUnificationError (Pos 1 1 1) "Number" "TQuant Instantiation fromList [] 2" rv

test_type_annotations_on_function_decls2 = do
    rv <- run $ T.unlines
        [ "fun id_int<a>(x: a): Number { x }"
        ]
    assertUnificationError (Pos 1 1 1) "Number" "TQuant Instantiation fromList [] 2" rv

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

test_row_variables_are_checked = do
    result <- run $ T.unlines
        [ "fun double_x(r) {"
        , "    r.x = r.x + r.x"
        , "    r"
        , "}"
        , ""
        , "let a = double_x({ x : 22, y : 11 })"
        , "let _ = print(a.z)"
        ]
    assertUnificationError (Pos 1 7 15) "{x: Number,y: Number}" "{z: (TUnbound fromList [] 34),..._35}" result
