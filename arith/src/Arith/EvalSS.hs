module Arith.EvalSS
    ( eval, eval1
    ) where

import Control.Monad.Except
import Arith.Syntax

-- Small-step evalutation
eval :: Term Info -> Either NoRuleApplies (Term Info)
eval t = if isVal t 
    then return t
    else case eval1 t of 
        Right t' -> eval t'
        error -> error

eval1 :: Term Info -> Either NoRuleApplies (Term Info)
eval1 t = case t of
    TmIf _ (TmTrue _) t2 t3 -> return t2
    TmIf _ (TmFalse _) t2 t3 -> return t3
    TmIf fi t1 t2 t3 -> case eval1 t1 of
                            Right t1' -> return $ TmIf fi t1' t2 t3
                            error -> error
    TmSucc fi t1 -> case eval1 t1 of
                        Right t1' -> return $ TmSucc fi t1'
                        error -> error
    TmPred _ (TmZero _) -> return $ TmZero DummyInfo
    TmPred _ (TmSucc _ nv1) | isNumericalVal nv1 -> return nv1
    TmPred fi t1 -> case eval1 t1 of
                        Right t1' -> return $ TmPred fi t1
                        error -> error
    TmIsZero _ (TmZero _) -> return $ TmTrue DummyInfo
    TmIsZero _ (TmSucc _ nv1) | isNumericalVal nv1 -> return $ TmFalse DummyInfo
    TmIsZero fi t1 -> case eval1 t1 of
                            Right t1' -> return $ TmIsZero fi t1'
                            error -> error
    _ -> throwError NoRuleApplies