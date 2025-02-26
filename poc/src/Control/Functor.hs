module Control.Functor where
--
import Control.Category

class (Category cat, Category cat') => Functor cat cat' m where
  cfmap :: cat a b -> cat' (m a) (m b)

type EndoFunctor cat = Functor cat cat
