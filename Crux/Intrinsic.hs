{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types        #-}

module Crux.Intrinsic where

import           Crux.AST
import qualified Crux.JSGen.Types    as JSGen
import qualified Crux.JSTree         as JS
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

data Intrinsic = Intrinsic
    { iType :: TypeVar
    , iGen  :: forall a. JSGen.GenVTable -> JSGen.Env -> Expression a -> JSGen.JSWrite JS.Expression
    }

genPlus :: JSGen.GenVTable -> JSGen.Env -> Expression t -> JSGen.JSWrite JS.Expression
genPlus JSGen.GenVTable{..} env (EApp _ (EIdentifier _ "+") [lhs, rhs]) = do
    lhs' <- vGenerateExpr env lhs
    rhs' <- vGenerateExpr env rhs
    return $ JS.EBinOp "+" lhs' rhs'
genPlus _ _ _ = error "Unexpected: Only pass EApp to genPlus"

intrinsics :: HashMap Name Intrinsic
intrinsics = HashMap.fromList
    [ ("+", Intrinsic
        { iType = TFun [TType Number, TType Number] (TType Number)
        , iGen = genPlus
        }
      )
    ]
