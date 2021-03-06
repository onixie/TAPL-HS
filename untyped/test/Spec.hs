{-# Language TemplateHaskell #-}

import Test.QuickCheck
import qualified Untyped.EvalSS as SS
import qualified Untyped.EvalBS as BS
import Untyped.Syntax

prop_printOrd1 = once $ showOrd [] term === "(λx.x) (λy.y y)"
    where term = TmApp NoInfo (TmAbs NoInfo "x" (TmVar NoInfo 1 0))
                              (TmAbs NoInfo "y" (TmApp NoInfo (TmVar NoInfo 1 0)
                                                              (TmVar NoInfo 1 0)))

prop_printOrd2 = once $ showOrd [] term === "(λz.(λy.z y)) (λx.x)"
    where term = TmApp NoInfo (TmAbs NoInfo "z" (TmAbs NoInfo "y" (TmApp NoInfo (TmVar NoInfo 2 1)
                                                                                (TmVar NoInfo 2 0))))
                              (TmAbs NoInfo "x" (TmVar NoInfo 1 0))

prop_printOrd3 = once $ showOrd [] term === "(λy.(λx.x) y)"
    where term = TmAbs NoInfo "y" (TmApp NoInfo (TmAbs NoInfo "x" (TmVar NoInfo 2 0))
                                                (TmVar NoInfo 1 0))

prop_eval_ss1 = once $ SS.eval [] term == Right expect
    where term = TmApp NoInfo (TmAbs NoInfo "x" (TmVar NoInfo 1 0))
                              (TmAbs NoInfo "y" (TmApp NoInfo (TmVar NoInfo 1 0)
                                                              (TmVar NoInfo 1 0)))
          expect = TmAbs NoInfo "y" (TmApp NoInfo (TmVar NoInfo 1 0)
                                                  (TmVar NoInfo 1 0))

prop_eval_bs1 = once $ BS.eval [] term == Right expect
    where term = TmApp NoInfo (TmAbs NoInfo "x" (TmVar NoInfo 1 0))
                              (TmAbs NoInfo "y" (TmApp NoInfo (TmVar NoInfo 1 0)
                                                              (TmVar NoInfo 1 0)))
          expect = TmAbs NoInfo "y" (TmApp NoInfo (TmVar NoInfo 1 0)
                                                  (TmVar NoInfo 1 0))

prop_eval_ss2 = once $ SS.eval [] term == Right expect
    where term = TmApp NoInfo (TmAbs NoInfo "y" (TmApp NoInfo (TmVar NoInfo 1 0)
                                                              (TmVar NoInfo 1 0)))
                              (TmAbs NoInfo "x" (TmVar NoInfo 1 0))
          expect = TmAbs NoInfo "x" (TmVar NoInfo 1 0)

prop_eval_bs2 = once $ BS.eval [] term == Right expect
    where term = TmApp NoInfo (TmAbs NoInfo "y" (TmApp NoInfo (TmVar NoInfo 1 0)
                                                              (TmVar NoInfo 1 0)))
                              (TmAbs NoInfo "x" (TmVar NoInfo 1 0))
          expect = TmAbs NoInfo "x" (TmVar NoInfo 1 0)

prop_eval_ss3 = once $ SS.eval [] term == Right expect
    where term = TmApp NoInfo (TmAbs NoInfo "z" (TmAbs NoInfo "y" (TmApp NoInfo (TmVar NoInfo 2 1)
                                                                                (TmVar NoInfo 2 0))))
                              (TmAbs NoInfo "x" (TmVar NoInfo 1 0))
          expect = TmAbs NoInfo "y" (TmApp NoInfo (TmAbs NoInfo "x" (TmVar NoInfo 2 0))
                                                  (TmVar NoInfo 1 0))

prop_eval_bs3 = once $ BS.eval [] term == Right expect
    where term = TmApp NoInfo (TmAbs NoInfo "z" (TmAbs NoInfo "y" (TmApp NoInfo (TmVar NoInfo 2 1)
                                                                                (TmVar NoInfo 2 0))))
                              (TmAbs NoInfo "x" (TmVar NoInfo 1 0))
          expect = TmAbs NoInfo "y" (TmApp NoInfo (TmAbs NoInfo "x" (TmVar NoInfo 2 0))
                                                  (TmVar NoInfo 1 0))

-- runTests
return []
main = $quickCheckAll