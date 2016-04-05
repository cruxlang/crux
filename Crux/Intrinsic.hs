{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}

module Crux.Intrinsic
    ( Intrinsic(..)
    , intrinsics
    ) where

import qualified Crux.AST            as AST
import qualified Crux.JSGen.Types    as JSGen
import qualified Crux.JSTree         as JS
import           Crux.Prelude
import           Crux.TypeVar
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

intrinsics :: HashMap AST.Name Intrinsic
intrinsics =
    let numTy = TPrimitive Number in
    let plusTy = TFun [numTy, numTy] numTy in
    HashMap.fromList
        [ ("+", Intrinsic
            { iType = plusTy
            , iGen = genPlus
            }
          )
        ]
