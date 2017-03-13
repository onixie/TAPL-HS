{-# Language TemplateHaskell #-}

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

-- properties
prop_ifZero = evalBy BS.eval "if ?0 false true" == evalBy SS.eval "if ?0 false true"
prop_succTo4 = evalBy BS.eval "1+1+1+1+0" == evalBy SS.eval "1+1+1+1+0"

-- runTests
return []
main = $quickCheckAll