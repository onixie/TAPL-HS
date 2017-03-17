module Arith.Parse (
      true
    , false
    , if_
    , zero
    , succ
    , pred
    , iszero
    , parse
    , term
    , terms
    ) where

import Prelude hiding (succ, pred)
import Arith.Syntax
import Text.Parsec

data ParseInfo = 
      ParseInfo SourcePos 
    | NoInfo deriving (Show, Eq)

instance Info ParseInfo where
    dummy = NoInfo

terms = many1 term

term = between spaces spaces (true <|> false <|> if_ <|> zero <|> try succ <|> pred <|> iszero)

true :: Parsec String () (Term ParseInfo)
true = TmTrue <$> (ParseInfo <$> getPosition) <* string "true"

false :: Parsec String () (Term ParseInfo)
false = TmFalse <$> (ParseInfo <$> getPosition) <* string "false"

if_ :: Parsec String () (Term ParseInfo)
if_ = TmIf <$> (ParseInfo <$> getPosition <* string "if") <*> term <*> term <*> term

zero :: Parsec String () (Term ParseInfo)
zero = TmZero <$> (ParseInfo <$> getPosition) <* char '0'

succ :: Parsec String () (Term ParseInfo)
succ = TmSucc <$> (ParseInfo <$> getPosition <* string "1+") <*> term

pred :: Parsec String () (Term ParseInfo)
pred = TmPred <$> (ParseInfo <$> getPosition <* string "1-") <*> term

iszero :: Parsec String () (Term ParseInfo)
iszero = TmIsZero <$> (ParseInfo <$> getPosition <* string "?") <*> term