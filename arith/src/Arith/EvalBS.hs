module Arith.EvalBS
    ( eval
    ) where

import Control.Monad.Except
import Arith.Syntax

-- Big-step evaluation
eval :: Info i => Term i -> Either (Error i) (Term i)
eval t = case t of
    t | isVal t -> return t
    TmIf fi t1 t2 t3 -> case eval t1 of
        Right (TmFalse _) -> let t3' = eval t3 in t3'
        Right (TmTrue _) -> let t2' = eval t2 in t2'
        Left e -> Left e
        _ -> throwError . NoRuleApplies $ fi
    TmSucc fi t1 -> case eval t1 of
        Right t1' | isNumericalVal t1' -> return $ TmSucc fi t1'
        Left e -> Left e
        _ -> throwError . NoRuleApplies $ fi
    TmPred fi t1 -> case eval t1 of
        Right (TmZero _) -> return $ TmZero dummy
        Right (TmSucc _ nv1) | isNumericalVal nv1 -> return nv1
        Left e -> Left e
        _ -> throwError . NoRuleApplies $ fi
    TmIsZero fi t1 -> case eval t1 of
        Right (TmZero _) -> return $ TmTrue dummy
        Right (TmSucc _ nv1) | isNumericalVal nv1 -> return $ TmFalse dummy
        Left e -> Left e
        _ -> throwError . NoRuleApplies $ fi
    _ -> undefined