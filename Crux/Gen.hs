{-# LANGUAGE DisambiguateRecordFields, RecordWildCards, GeneralizedNewtypeDeriving, FlexibleInstances, OverloadedStrings #-}

module Crux.Gen where

import Crux.AST

import Data.Monoid ((<>))
import Control.Monad.State.Lazy
import Control.Applicative
import Data.Default (Default(def))
import Data.Word (Word)
import Data.Text (Text, unpack)
import qualified Data.Text as T

import LLVM.General.AST
import LLVM.General.AST.Global
import qualified LLVM.General.AST.AddrSpace as A
import qualified LLVM.General.AST.CallingConvention as C
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Visibility as V
import qualified LLVM.General.AST.Instruction as I
import qualified LLVM.General.AST.Linkage as L
import qualified LLVM.General.AST.Name as N
import qualified LLVM.General.AST.Operand as O
import qualified LLVM.General.AST.Type as T

data CodeGenerator = CodeGenerator
    { nextName :: Word
    , definitions :: [Definition]
    }

instance Default CodeGenerator where
    def = CodeGenerator { nextName = def, definitions = def }

newtype CodeGen a = CodeGen { runCodeGen :: State CodeGenerator a }
    deriving (Functor, Applicative, Monad, MonadState CodeGenerator)

mkName :: CodeGen Name
mkName = do
    cg <- get
    let CodeGenerator{..} = cg
    let name = N.UnName nextName
    put cg { nextName = nextName + 1 }
    return name

mkLongName :: String -> CodeGen Name
mkLongName prefix = do
    N.UnName n <- mkName
    return $ N.Name (prefix ++ (show n))

addDefinition :: Definition -> CodeGen ()
addDefinition defn = do
    cg <- get
    let CodeGenerator{..} = cg
    put cg { definitions = definitions ++ [defn] }

charPointerType = PointerType (IntegerType 8) (A.AddrSpace 0)

charArrayType len = ArrayType (fromIntegral len) (IntegerType 8)

declarePrintf :: Definition
declarePrintf =
    GlobalDefinition $ functionDefaults
        { name = Name "puts"
        , parameters =
            let typ = charPointerType
            in ([Parameter typ (N.Name "s") []], False)
        , returnType = IntegerType 32
        , basicBlocks = []
        }

statement (EPrint p) = printStatement p

printStatement (LString str) = do
    stringName <- stringConstant str
    castName <- mkName
    callName <- mkName
    return
        [ castName := I.BitCast
            { operand0=O.ConstantOperand $ C.GlobalReference (charArrayType $ T.length str) stringName
            , type'=charPointerType
            , metadata=[]
            }
        , callName := I.Call
            { isTailCall=False
            , callingConvention=C.C
            , returnAttributes=[]
            , function=(Right $ O.ConstantOperand $ C.GlobalReference T.VoidType (N.Name "puts"))
            , arguments=[(O.LocalReference undefined castName, [])]
            , functionAttributes=[]
            , metadata=[]
            }
        ]

stringConstant :: Text -> CodeGen Name
stringConstant s = do
    name <- mkLongName "str"
    let s' = s <> "\n\0"
    let values = [C.Int 8 (fromIntegral $ fromEnum c) | c <- unpack s']
    addDefinition $ GlobalDefinition $ GlobalVariable
        { name=name
        , linkage=L.Private
        , visibility=V.Default
        , isThreadLocal=False
        , addrSpace=(A.AddrSpace 0)
        , hasUnnamedAddr=True
        , isConstant=True
        , type'=(charArrayType $ fromIntegral $ T.length s')
        , initializer=(Just $ C.Array (IntegerType 8) values)
        , section=Nothing
        , alignment=0
        }
    return name

gen :: String -> [Expression] -> Module
gen moduleName expressions = do
    let gs = flip (execState . runCodeGen) def $ do
            addDefinition declarePrintf
            blockName <- mkName
            statements <- forM expressions $ \expr -> do
                statement expr

            retName <- mkName
            addDefinition $ GlobalDefinition $ functionDefaults
                { name = N.Name "main"
                , parameters = ([], False)
                , returnType = T.VoidType
                , basicBlocks =
                    [ BasicBlock blockName
                        (foldr (++) [] statements)
                        (retName := I.Ret Nothing [])
                    ]
                }

    defaultModule
        { moduleName = moduleName
        , moduleDefinitions = definitions gs
        }
