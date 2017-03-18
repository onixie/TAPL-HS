{-# Language TemplateHaskell #-}

import Test.QuickCheck
import qualified Untyped.EvalSS as SS
import Untyped.Syntax

prop_printOrd1 = showOrd [] term === "(λx.x) (λy.y y)"
    where term = TmApp NoInfo (TmAbs NoInfo "x" (TmVar NoInfo 1 0))
                              (TmAbs NoInfo "y" (TmApp NoInfo (TmVar NoInfo 1 0)
                                                              (TmVar NoInfo 1 0)))

prop_printOrd2 = showOrd [] term === "(λz.(λy.z y)) (λx.x)"
    where term = TmApp NoInfo (TmAbs NoInfo "z" (TmAbs NoInfo "y" (TmApp NoInfo (TmVar NoInfo 2 1)
                                                                                (TmVar NoInfo 2 0))))
                              (TmAbs NoInfo "x" (TmVar NoInfo 1 0))

prop_printOrd3 = showOrd [] term === "(λy.(λx.x) y)"
    where term = TmAbs NoInfo "y" (TmApp NoInfo (TmAbs NoInfo "x" (TmVar NoInfo 2 0))
                                                (TmVar NoInfo 1 0))

prop_eval_ss1 = SS.eval [] term == Right expect
    where term = TmApp NoInfo (TmAbs NoInfo "x" (TmVar NoInfo 1 0))
                              (TmAbs NoInfo "y" (TmApp NoInfo (TmVar NoInfo 1 0)
                                                              (TmVar NoInfo 1 0)))
          expect = TmAbs NoInfo "y" (TmApp NoInfo (TmVar NoInfo 1 0)
                                                  (TmVar NoInfo 1 0))

prop_eval_ss2 = SS.eval [] term == Right expect
    where term = TmApp NoInfo (TmAbs NoInfo "y" (TmApp NoInfo (TmVar NoInfo 1 0)
                                                              (TmVar NoInfo 1 0)))
                              (TmAbs NoInfo "x" (TmVar NoInfo 1 0))
          expect = TmAbs NoInfo "x" (TmVar NoInfo 1 0)

prop_eval_ss3 = SS.eval [] term == Right expect
    where term = TmApp NoInfo (TmAbs NoInfo "z" (TmAbs NoInfo "y" (TmApp NoInfo (TmVar NoInfo 2 1)
                                                                                (TmVar NoInfo 2 0))))
                              (TmAbs NoInfo "x" (TmVar NoInfo 1 0))
          expect = TmAbs NoInfo "y" (TmApp NoInfo (TmAbs NoInfo "x" (TmVar NoInfo 2 0))
                                                  (TmVar NoInfo 1 0))

-- runTests
return []
main = $quickCheckAll
