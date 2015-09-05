{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types        #-}

module Sneak.Intrinsic
    ( Intrinsic(..)
    , intrinsics
    ) where

import Sneak.Prelude
import qualified Sneak.AST as AST
import qualified Sneak.JSGen.Types    as JSGen
import qualified Sneak.JSTree         as JS
import qualified Data.HashMap.Strict as HashMap

data Intrinsic = Intrinsic
    { iType :: AST.TypeVar
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
    numTy <- newIORef $ AST.MutableTypeVar $ AST.TPrimitive AST.Number
    plusTy <- newIORef $ AST.MutableTypeVar $ AST.TFun [numTy, numTy] numTy
    return $ HashMap.fromList
        [ ("+", Intrinsic
            { iType = plusTy
            , iGen = genPlus
            }
          )
        ]
