module Data.Num where
--
import Control.Category

class Num k a | a -> k where
  (+)    :: forall r. O2 k r a => k r a -> k r a -> k r a
  (-)    :: forall r. O2 k r a => k r a -> k r a -> k r a
  (*)    :: forall r. O2 k r a => k r a -> k r a -> k r a
  negate :: forall r. O2 k r a => k r a -> k r a
  abs    :: forall r. O2 k r a => k r a -> k r a
  signum :: forall r. O2 k r a => k r a -> k r a
