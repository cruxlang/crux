{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types        #-}

module Crux.Intrinsic where

import qualified Crux.AST as AST
import qualified Crux.JSGen.Types    as JSGen
import qualified Crux.JSTree         as JS
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

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

intrinsics :: HashMap AST.Name Intrinsic
intrinsics = HashMap.fromList
    [ ("+", Intrinsic
        { iType = AST.TFun [AST.TType AST.Number, AST.TType AST.Number] (AST.TType AST.Number)
        , iGen = genPlus
        }
      )
    ]
