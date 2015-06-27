{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module Crux.Intrinsic where

import           Crux.AST
import qualified Crux.JSGen.Types    as JSGen
import qualified Crux.JSTree         as JS
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

data Intrinsic = Intrinsic
    { iType :: TypeVar
    , iGen  :: forall a. JSGen.Env -> Expression a -> JSGen.JSWrite JS.Expression
    }

-- genPlus :: JSGen.Env -> Expression t -> JSGen.JSWrite JS.Expression
-- genPlus env (EApp _ (EApp _ _ lhs) rhs) = do
--     lhs' <- JSGen.generateExpr env lhs
--     rhs' <- JSGen.generateExpr env rhs
--     return $ JS.EBinOp "+" lhs' rhs'
-- genPlus _ _ = error "Unexpected: Only pass EApp to genPlus"

mkCurriedFunctionType :: [TypeVar] -> TypeVar -> TypeVar
mkCurriedFunctionType [] res = res
mkCurriedFunctionType (ty:rest) res = TFun [ty] (mkCurriedFunctionType rest res)

intrinsics :: HashMap Name Intrinsic
intrinsics = HashMap.fromList
    [ ("+", Intrinsic
        { iType = mkCurriedFunctionType [TType Number, TType Number] (TType Number)
        , iGen = undefined -- genPlus
        }
      )
    ]
