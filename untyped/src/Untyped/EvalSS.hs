module Untyped.EvalSS
    ( eval
    ) where

import Untyped.Syntax

-- Evaluation
eval :: Context -> Term -> Either (Error Info) Term
eval ctx t = if isVal t
    then Right t
    else case eval1 ctx t of
        Right t' -> eval ctx t'
        e -> e

eval1 :: Context -> Term -> Either (Error Info) Term
eval1 ctx t = case t of
    TmApp _ (TmAbs _ _ t12) v2 | isVal v2 ->
        Right $ shiftTop (substitute (shiftTop v2 1) 0 t12) (-1)
    TmApp fi v1 t2 | isVal v1 -> case eval1 ctx t2 of
        Right t2' -> Right $ TmApp fi v1 t2'
        e -> e
    TmApp fi t1 t2 -> case eval1 ctx t1 of
        Right t1' -> Right $ TmApp fi t1' t2
        e -> e
    _ -> Left $ NoRuleApplies NoInfo