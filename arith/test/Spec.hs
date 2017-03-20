{-# Language TemplateHaskell, FlexibleInstances, ScopedTypeVariables, FlexibleContexts #-}

import Control.Monad
import Test.QuickCheck
import Arith.Syntax
import Arith.Parse
import qualified Arith.EvalBS as BS
import qualified Arith.EvalSS as SS

-- helper
evalBy eval text = case parse term "" text of
    Right term -> eval term
    Left _ -> Left . WrongSyntax $ dummy

data DummyInfo = DummyInfo deriving (Show, Eq)

instance Info DummyInfo where
    dummy = DummyInfo

instance Arbitrary DummyInfo where
    arbitrary = return DummyInfo

instance Arbitrary (Term DummyInfo) where
    arbitrary = sized term
        where term 1 = oneof [ TmFalse <$> arbitrary
                             , TmTrue <$> arbitrary
                             , TmZero <$> arbitrary
                             ]
              term n | n > 1 = 
                       oneof [ TmSucc <$> arbitrary <*> (subterm n)
                             , TmPred <$> arbitrary <*> (subterm n)
                             , TmIsZero <$> arbitrary <*> (subterm n)
                             , TmIf <$> arbitrary <*> (subterm n) <*> (subterm n) <*> (subterm n)
                             ]
                where subterm n = term (n `div` 2)

ssEqualBs :: Term DummyInfo -> Property
ssEqualBs arb = collect (depth arb)
              $ classify (good ss) "good"
              $ classify (not (good ss)) "bad"
              $ ss == bs
    where ss = SS.eval arb
          bs = BS.eval arb

good (Right _) = True
good _ = False

bothGood t = good (BS.eval t) && good (SS.eval t)

depth :: Info i => Term i -> Int
depth (TmFalse _) = 1
depth (TmTrue _) = 1
depth (TmZero _) = 1
depth (TmIsZero _ t1) = 1 + depth t1
depth (TmSucc _ t1) = 1 + depth t1
depth (TmPred _ t1) = 1 + depth t1
depth (TmIf _ t1 t2 t3) = 1 + (depth t1 `max` depth t2 `max` depth t3)

-- properties
prop_ifZero = once $ evalBy BS.eval "if ?0 false true" == evalBy SS.eval "if ?0 false true"
prop_succTo4 = once $ evalBy BS.eval "1+1+1+1+0" == evalBy SS.eval "1+1+1+1+0"
prop_arbitrary_good = forAll (arbitrary `suchThat` bothGood) ssEqualBs
prop_arbitrary_bad = forAll (arbitrary `suchThat` (not . bothGood)) ssEqualBs

-- runTests
return []
main = $quickCheckAll