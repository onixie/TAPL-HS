module Untyped.Syntax
    ( Term (..)
    , Error (..)
    , Info (..)
    , Binding (..)
    , showOrd
    , printOrd
    , Context
    , DeBrujinIndex
    , shift
    , shiftTop
    , substitute
    , isVal
    ) where

import Prelude hiding (print)
import Data.List()

-- Nameless Representation

type ContextLength = Int
type DeBrujinIndex = Int
type VarName = String

data Term = TmVar Info ContextLength DeBrujinIndex
          | TmAbs Info VarName Term
          | TmApp Info Term Term
          deriving (Show, Eq)

data Error i = 
      WrongSyntax i
    | NoRuleApplies i deriving (Show, Eq)

data Info = NoInfo
          deriving (Show, Eq)

type Context = [(VarName, Binding)]
data Binding = NoBind

-- print as Ordinary Representation

showOrd :: Context -> Term -> String
showOrd ctx t = case t of
    TmAbs fi x t1 -> let ctx'@((x', _):_) = pickFreshName ctx x in
        "(λ" ++ x' ++ "." ++ showOrd ctx' t1 ++ ")"
    TmApp fi t1 t2 -> showOrd ctx t1 ++ " " ++ showOrd ctx t2
    TmVar fi n x -> if length ctx == n
                    then fst $ ctx !! x
                    else "[bad index]"

printOrd :: Context -> Term -> IO ()
printOrd ctx = putStrLn . showOrd ctx

pickFreshName :: Context -> VarName -> Context
pickFreshName ctx x = let x' = if exist then x ++ "'" else x 
                      in (x', NoBind):ctx
  where exist = elem x $ map fst ctx

-- shift and substitution

shift :: Term -> Int -> Int -> Term
shift t d c = case t of
    TmVar i n k | k < c -> TmVar i (n + d) k
                | k >=c -> TmVar i (n + d) (k + d)
    TmAbs i x t1 -> TmAbs i x (shift t1 d (c + 1))
    TmApp i t1 t2 -> TmApp i (shift t1 d c) (shift t2 d c)

shiftTop t d = shift t d 0

substitute :: Term -> DeBrujinIndex -> Term -> Term
substitute s j t = case t of
    TmVar i n k | k == j -> s
                | otherwise -> TmVar i n k
    TmAbs i x t1 -> TmAbs i x (substitute (shiftTop s 1) (j + 1) t1)
    TmApp i t1 t2 -> TmApp i (substitute s j t1) (substitute s j t2)

isVal t = case t of
    TmAbs _ _ _ -> True
    _ -> False