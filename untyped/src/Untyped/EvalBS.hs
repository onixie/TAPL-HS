module Untyped.EvalBS
    ( eval
    ) where

import Untyped.Syntax

-- Evaluation
eval :: Context -> Term -> Either (Error Info) Term
eval ctx t = if isVal t
    then Right t
    else case t of
        TmApp _ t1 t2 -> case eval ctx t1 of
            Right (TmAbs _ _ t12) -> case eval ctx t2 of
                Right v2 | isVal v2 -> eval ctx $ shiftTop (substitute (shiftTop v2 1) 0 t12) (-1)
                _ -> Left $ NoRuleApplies NoInfo
            _ -> Left $ NoRuleApplies NoInfo
        _ -> Left $ NoRuleApplies NoInfo