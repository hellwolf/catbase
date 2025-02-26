module Control.Category where

import Data.Kind (Type, Constraint)


class Category (cat :: Type -> Type -> Type) where
  type family Object cat :: Type -> Constraint

  -- \| The identity morphism. Implementations should satisfy two laws:
  --
  -- [Right identity] @f '.' 'id'  =  f@
  -- [Left identity]  @'id' '.' f  =  f@
  --
  -- These essentially state that 'id' should "do nothing".
  id :: forall a. O1 cat a => cat a a

  -- | Morphism composition. Implementations should satisfy the law:
  --
  -- [Associativity]  @f '.' (g '.' h)  =  (f '.' g) '.' h@
  --
  -- This means that the way morphisms are grouped is irrelevant, so it is unambiguous
  -- to write a composition of morphisms as @f '.' g '.' h@, without parentheses.
  (.) :: forall a b c. O3 cat a b c => cat b c -> cat a b -> cat a c

type O1 k a1 = Object k a1 :: Constraint
type O2 k a1 a2 = (O1 k a2, O1 k a1)
type O3 k a1 a2 a3 = (O2 k a2 a3, O1 k a1)
type O4 k a1 a2 a3 a4 = (O3 k a2 a3 a4, O1 k a1)
type O5 k a1 a2 a3 a4 a5 = (O4 k a2 a3 a4 a5, O1 k a1)
