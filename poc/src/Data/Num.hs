module Data.Num where
import Prelude.Base qualified as Base


class Num a where
  (+)    :: a -> a -> a
  (-)    :: a -> a -> a
  (*)    :: a -> a -> a
  negate :: a -> a
  abs    :: a -> a
  signum :: a -> a
  fromInteger :: Base.Integer -> a

instance Num (Base.Int) where
  (+) = (Base.+)
  (-) = (Base.-)
  (*) = (Base.*)
  negate = Base.negate
  abs = Base.abs
  signum = Base.signum
  fromInteger = Base.fromInteger