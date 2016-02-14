{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types        #-}

module Crux.Intrinsic
    ( Intrinsic(..)
    , intrinsics
    ) where

import Crux.Prelude
import Crux.TypeVar
import qualified Crux.AST as AST
import qualified Crux.JSGen.Types    as JSGen
import qualified Crux.JSTree         as JS
import qualified Data.HashMap.Strict as HashMap

data Intrinsic = Intrinsic
    { iType :: TypeVar
    , iGen  :: forall a. JSGen.GenVTable -> JSGen.Env -> AST.Expression Text a -> JSGen.JSWrite JS.Expression
    }

genPlus :: JSGen.GenVTable -> JSGen.Env -> AST.Expression Text t -> JSGen.JSWrite JS.Expression
genPlus JSGen.GenVTable{..} env (AST.EApp _ (AST.EIdentifier _ "+") [lhs, rhs]) = do
    lhs' <- vGenerateExpr env lhs
    rhs' <- vGenerateExpr env rhs
    return $ JS.EBinOp "+" lhs' rhs'
genPlus _ _ _ = error "Unexpected: Only pass EApp to genPlus"

intrinsics :: IO (HashMap AST.Name Intrinsic)
intrinsics = do
    numTy <- newIORef $ TPrimitive Number
    plusTy <- newIORef $ TFun [numTy, numTy] numTy
    return $ HashMap.fromList
        [ ("+", Intrinsic
            { iType = plusTy
            , iGen = genPlus
            }
          )
        ]
