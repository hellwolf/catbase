{-# OPTIONS_GHC -Wno-orphans #-}
module Hask.Types.Int where
-- ghc-prims
import GHC.Types qualified
import Prelude.Base qualified as Base
--
import Control.Category.Hask
import Data.Num


type Int' = GHC.Types.Int
type Int = Hask () Int'
instance HaskObject Int'

instance Num (Hask r Int') where
  (MkHask rx) + (MkHask ry) = MkHask \r -> rx r Base.+ ry r
  (MkHask rx) - (MkHask ry) = MkHask \r -> rx r Base.- ry r
  (MkHask rx) * (MkHask ry) = MkHask \r -> rx r Base.* ry r
  negate (MkHask rx) = MkHask \r -> Base.negate (rx r)
  abs (MkHask rx) = MkHask \r -> Base.abs (rx r)
  signum (MkHask rx) = MkHask \r -> Base.signum (rx r)
