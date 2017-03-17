module Untyped.Syntax
    ( Term (..)
    , Info (..)
    ) where

data Info = VarName String
          | ContextDepth Int
          | Dummy
          deriving (Show, Eq)

data Term = TmVar Info Int        -- Variable (Context Depth) * (De Brujin Index)
          | TmAbs Info Term       -- Abstraction (Bound Variable Name) * (Enclosed Term)
          | TmApp Info Term Term  -- Application (Left Term) * (Right Term)
          deriving (Show, Eq)
