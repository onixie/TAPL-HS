module Arith.Syntax
    ( Term (..),
      Info (..),
      isNumericalVal,
      isVal,
      NoRuleApplies (..)
    ) where

-- Syntax

data Info = DummyInfo deriving (Show, Eq)

data Term info = 
      TmTrue info
    | TmFalse info
    | TmIf info (Term info) (Term info) (Term info)
    | TmZero info
    | TmSucc info (Term info)
    | TmPred info (Term info)
    | TmIsZero info (Term info)
    deriving (Show, Eq)

data NoRuleApplies = NoRuleApplies deriving (Show, Eq)

isNumericalVal t = case t of
    TmZero _ -> True
    TmSucc _ t1 -> isNumericalVal t1
    _ -> False

isVal t = case t of
    TmTrue _ -> True
    TmFalse _ -> True
    t | isNumericalVal t -> True
    _ -> False
