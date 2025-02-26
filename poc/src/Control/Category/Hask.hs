{-# LANGUAGE UndecidableInstances #-}
module Control.Category.Hask where
-- base
import Prelude.Base qualified as Base
--
import Control.Category
import Control.Functor


newtype Hask a b = MkHask { unHask :: a -> b }
mkHaskObject :: forall r a. O2 Hask r a => a -> Hask r a
mkHaskObject a = MkHask (\_ -> a)

class HaskObject a

instance Category Hask where
  type instance Object Hask = HaskObject

  id = MkHask (\a -> a)

  (MkHask bc) . (MkHask ab) = MkHask (\a -> bc (ab a))

-- | Initial Hask object.
data Empty
instance HaskObject Empty

-- | Terminal Hask object.
instance HaskObject ()

-- | List object.
instance O1 Hask a => HaskObject [a]

-- | All Hask functors are endo-functors.
instance Base.Functor m => Functor Hask Hask m where
  cfmap (MkHask f) = MkHask \ma -> Base.fmap f ma

-- TODO, make varidic calls

runHask :: O2 Hask a b => Hask a b -> a -> b
runHask m a = unHask m a

runHask1 :: O2 Hask a b =>
  (forall r. O1 Hask r => Hask r a -> Hask r b) ->
  (a -> b)
runHask1 f a = unHask (f (mkHaskObject a) . mkHaskObject ()) ()

runHask2 :: O3 Hask a b c =>
  (forall r. O1 Hask r => Hask r a -> Hask r b -> Hask r c) ->
  (a -> b -> c)
runHask2 f a b = unHask (f (mkHaskObject a) (mkHaskObject b) . mkHaskObject ()) ()
