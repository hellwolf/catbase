{-# OPTIONS_GHC -Wno-orphans #-}
module Hask.Types.Int where
-- ghc-prims
import Prelude.Base qualified as Base
--
import Control.Category.Hask
import Data.Num


type Int' = Base.Int
type Int = H Int'
instance HaskObject Int'

instance Num Int where
  hx + hy = hEmb (hVal hx Base.+ hVal hy)
  hx - hy = hEmb (hVal hx Base.- hVal hy)
  hx * hy = hEmb (hVal hx Base.* hVal hy)
  negate hx = hEmb (Base.negate (hVal hx))
  abs hx = hEmb (Base.abs (hVal hx))
  signum hx = hEmb (Base.signum (hVal hx))
  fromInteger x = hEmb (Base.fromInteger x)
