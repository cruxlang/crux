{-# LANGUAGE OverloadedStrings #-}

module IntegrationTest (run, tests) where

import           Control.Monad  (forM)
import           Crux.JSGen
import qualified Crux.JSTree    as JSTree
import           Crux.Lex
import           Crux.Parse
import qualified Crux.Typecheck as Typecheck
import           Data.Text      (Text)
import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import           System.Process (readProcess)
import           Test.HUnit
import qualified Data.Text as T
-- import           Text.Show.Pretty   (ppShow)
import           Data.Monoid    ((<>))

run :: Text -> IO (Either String Text)
run src = do
    let fn = "<string>"
    let l = Crux.Lex.lexSource fn src
    case l of
        Left err ->
            return $ Left $ "Lex error: " <> show err
        Right l' -> do
            p <- Crux.Parse.parse fn l'
            case p of
                Left err -> return $ Left $ "Parse error: " ++ show err
                Right p' -> do
                    typetree <- Typecheck.run p'
                    typetree' <- forM typetree Typecheck.flattenDecl
                    T.writeFile "temp.js" $ JSTree.renderDocument (concatMap generateDecl typetree')

                    fmap (Right . T.pack) $ readProcess "node" ["temp.js"] ""

testHelloWorld = do
    result <- run $ T.unlines
        [ "let _ = print \"Hello, World!\";"
        ]
    assertEqual "" (Right "Hello, World!\n") result

testInteger = do
    result <- run $ T.unlines
        [ "let x = 1;"
        , "let y = x;"
        , "let _ = print (toString y);"
        ]
    assertEqual "" (Right "1\n") result

testDataTypes = do
    result <- run $ T.unlines
        [ "data IntList {"
        , "    Element Int IntList;"
        , "    Nil;"
        , "};"
        , "let mylist = Element 1 (Element 2 Nil);"
        , "let _ = print mylist"
        ]

    assertEqual "" (Right "[ 'Element', 1, [ 'Element', 2, [ 'Nil' ] ] ]\n") result

tests :: Test
tests = TestList
    [ TestLabel "testHelloWorld" $ TestCase testHelloWorld
    , TestLabel "testInteger" $ TestCase testInteger
    ]
