module Arith.EvalBS
    ( eval
    ) where

import Control.Monad.Except
import Arith.Syntax

-- Big-step evaluation
eval :: Term Info -> Either NoRuleApplies (Term Info)
eval t = case t of
    t | isVal t -> return t
    TmIf _ t1 t2 t3 -> case eval t1 of
        Right (TmFalse _) -> let t3' = eval t3 in t3'
        Right (TmTrue _) -> let t2' = eval t2 in t2'
        _ -> throwError NoRuleApplies
    TmSucc fi t1 -> case eval t1 of
        Right t1' | isNumericalVal t1' -> return $ TmSucc fi t1'
        _ -> throwError NoRuleApplies
    TmPred fi t1 -> case eval t1 of
        Right (TmZero _) -> return $ TmZero DummyInfo
        Right (TmSucc _ nv1) | isNumericalVal nv1 -> return nv1
        _ -> throwError NoRuleApplies
    TmIsZero fi t1 -> case eval t1 of
        Right (TmZero _) -> return $ TmTrue DummyInfo
        Right (TmSucc _ nv1) | isNumericalVal nv1 -> return $ TmFalse DummyInfo
        _ -> throwError NoRuleApplies
    _ -> throwError NoRuleApplies