module Main where
-- base
import Prelude.Base qualified as Base
-- catbase
import Prelude.Catbase
import Hask


f :: forall a. Num a => a -> a -> a
f a b = a + a * b

main :: Base.IO ()
main = do
  Base.print (runHask2 f (3 :: Int') (2 :: Int') :: Int')
  Base.print (runHask (cfmap (MkHask (Base.+ (3 :: Int')))) ([1,2,3] :: [Int']))