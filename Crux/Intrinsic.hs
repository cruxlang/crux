{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types        #-}

module Crux.Intrinsic where

import qualified Crux.AST as AST
import qualified Crux.JSGen.Types    as JSGen
import qualified Crux.JSTree         as JS
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef (newIORef)

data Intrinsic = Intrinsic
    { iType :: AST.TypeVar
    , iGen  :: forall a. JSGen.GenVTable -> JSGen.Env -> AST.Expression a -> JSGen.JSWrite JS.Expression
    }

genPlus :: JSGen.GenVTable -> JSGen.Env -> AST.Expression t -> JSGen.JSWrite JS.Expression
genPlus JSGen.GenVTable{..} env (AST.EApp _ (AST.EIdentifier _ "+") [lhs, rhs]) = do
    lhs' <- vGenerateExpr env lhs
    rhs' <- vGenerateExpr env rhs
    return $ JS.EBinOp "+" lhs' rhs'
genPlus _ _ _ = error "Unexpected: Only pass EApp to genPlus"

intrinsics :: IO (HashMap AST.Name Intrinsic)
intrinsics = do
    numTy <- newIORef $ AST.TPrimitive AST.Number
    plusTy <- newIORef $ AST.TFun [numTy, numTy] numTy
    return $ HashMap.fromList
        [ ("+", Intrinsic
            { iType = plusTy
            , iGen = genPlus
            }
          )
        ]
