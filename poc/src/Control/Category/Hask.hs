module Control.Category.Hask where

import Control.Category

class HaskObject a

-- | Initial Hask object.
data Empty
instance HaskObject Empty

-- | Terminal Hask object.
instance HaskObject ()

newtype Hask a b = MkHask { unHask :: a -> b }

mkHaskObject :: forall r a. O2 Hask r a => a -> Hask r a
mkHaskObject a = MkHask (\_ -> a)

instance Category Hask where
  type instance Object Hask = HaskObject

  id = MkHask (\a -> a)

  (MkHask bc) . (MkHask ab) = MkHask (\a -> bc (ab a))

-- TODO, make varidic calls

runHask0 :: O1 Hask a =>
  (forall r. O1 Hask r => Hask r a) ->
  a
runHask0 f = unHask (f . mkHaskObject ()) ()

runHask1 :: O2 Hask a b =>
  (forall r. O1 Hask r => Hask r a -> Hask r b) ->
  (a -> b)
runHask1 f a = unHask (f (mkHaskObject a) . mkHaskObject ()) ()

runHask2 :: O3 Hask a b c =>
  (forall r. O1 Hask r => Hask r a -> Hask r b -> Hask r c) ->
  (a -> b -> c)
runHask2 f a b = unHask (f (mkHaskObject a) (mkHaskObject b) . mkHaskObject ()) ()
