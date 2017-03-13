module Arith.EvalSS
    ( eval
    ) where

import Control.Monad.Except
import Arith.Syntax

-- Small-step evalutation
eval :: Info i => Term i -> Either (Error i) (Term i)
eval t = if isVal t 
    then return t
    else case eval1 t dummy of 
        Right t' -> eval t'
        error -> error

eval1 :: Info i => Term i -> i -> Either (Error i) (Term i)
eval1 t fi = case t of
    TmIf _ (TmTrue _) t2 t3 -> return t2
    TmIf _ (TmFalse _) t2 t3 -> return t3
    TmIf fi t1 t2 t3 -> case eval1 t1 fi of
                            Right t1' -> return $ TmIf fi t1' t2 t3
                            error -> error
    TmSucc fi t1 -> case eval1 t1 fi of
                        Right t1' -> return $ TmSucc fi t1'
                        error -> error
    TmPred _ (TmZero _) -> return $ TmZero dummy
    TmPred _ (TmSucc _ nv1) | isNumericalVal nv1 -> return nv1
    TmPred fi t1 -> case eval1 t1 fi of
                        Right t1' -> return $ TmPred fi t1
                        error -> error
    TmIsZero _ (TmZero _) -> return $ TmTrue dummy
    TmIsZero _ (TmSucc _ nv1) | isNumericalVal nv1 -> return $ TmFalse dummy
    TmIsZero fi t1 -> case eval1 t1 fi of
                            Right t1' -> return $ TmIsZero fi t1'
                            error -> error
    _ -> throwError . NoRuleApplies $ fi