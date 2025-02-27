module Main where
-- base
import Prelude.Base qualified as Base
-- catbase
import Prelude.Catbase
import Hask


f :: forall a. Num a => a -> a -> a
f a b = a + a * b

main :: Base.IO ()
main = Base.do
  Base.print (runHask (f 3 2 :: Int) empty)
  Base.print (runHask (cfmap (hFn (+ (3 :: Int)))) [1,2,3])
