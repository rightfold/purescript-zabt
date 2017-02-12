
module Zabt.Nameless where

import Prelude
import Zabt.Index

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

derive instance eqNameless :: (Eq v, Eq (f x), Eq x) => Eq (Nameless v f x)
derive instance ordNameless :: (Ord v, Ord (f x), Ord x) => Ord (Nameless v f x)

instance showNameless :: (Show v, Show (f x), Show x) => Show (Nameless v f x) where
  show (Free v) = "(Free " <> show v <> ")"
  show (Bound i) = "(Bound " <> show i <> ")"
  show (Pattern f) = "(Pattern " <> show f <> ")"
  show (Abstraction s) = "(Abstraction " <> show s <> ")"
