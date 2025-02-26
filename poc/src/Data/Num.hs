module Data.Num where
--
import Control.Category

class Num a where
  (+)    :: a -> a -> a
  (-)    :: a -> a -> a
  (*)    :: a -> a -> a
  negate :: a -> a
  abs    :: a -> a
  signum :: a -> a
