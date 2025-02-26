module Main where
-- base
import Prelude.Base (type (~))
import Prelude.Base qualified as Base
-- catbase
import Prelude.Catbase
import Hask


-- NOTE! This function works for any category k!!
--
-- TODO: this signature could be simplified to:
-- @Num k a => Fn k (a -> a -> a)@
f :: forall a k r a'. (a ~ k r a', O2 k r a', Num k a') => a -> a -> a
f a b = a + a * b

main :: Base.IO ()
main = do
  -- Let's run @f@ in the category of Hask
  let b = runHask2 f (3 :: Int') (2 :: Int') :: Int'
  Base.print b
