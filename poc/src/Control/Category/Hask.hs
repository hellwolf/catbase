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

-- | Morphism from initial object.
type H a = Hask Empty a

-- | Embedding the hask value through the initial object.
hEmb :: a -> H a
hEmb x = MkHask \_ -> x

-- | Extract the hask value from its "initial morphism".
hVal :: H a -> a
hVal (MkHask fa) = fa Base.undefined

-- | Terminal Hask object.
instance HaskObject ()

-- | List object.
instance O1 Hask a => HaskObject [a]

-- | All Hask functors are endo-functors.
instance Base.Functor m => Functor Hask Hask m where
  cfmap (MkHask f) = MkHask \ma -> Base.fmap f ma

-- | Create a 2-ary Hask function from two "initial morphisms".
hFn :: O2 Hask a b => (H a -> H b) -> Hask a b
hFn f = MkHask \a -> let hb = f (MkHask \_ -> a) in hVal hb

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
