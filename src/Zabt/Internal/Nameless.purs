
module Zabt.Internal.Nameless where

import Zabt.Internal.Index

data Scope v x = Scope v x

instance showScope :: (Show v, Show x) => Show (Scope v x) where
  show (Scope v x) = "(Scope " <> show v <> " " <> show x <> ")"

instance eqScope :: (Eq x) => Eq (Scope v x) where
  eq (Scope _ x) (Scope _ y) = x == y

instance ordScope :: (Ord x) => Ord (Scope v x) where
  compare (Scope _ x) (Scope _ y) = x `compare` y

data Nameless v f x
  = Free v
  | Bound Index
  | Pattern (f x)
  | Abstraction (Scope v x)

derive instance eqNameless :: Eq Nameless
derive instance ordNameless :: Ord Nameless

instance showNameless :: (Show v, Show (f x), Show x) => Show (Nameless v f x) where
  showsPrec p (Free v) = showsPrec 11 v
  showsPrec p (Bound i) = showString "'" . showsPrec 11 i
  showsPrec p (Pattern f) = showsPrec p f
  showsPrec p (Abstraction (Scope v t)) = showParen (p >= 11) $
      showString "λ"
    . showSpace
    . showsPrec 11 t
