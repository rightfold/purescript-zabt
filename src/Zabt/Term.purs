
module Zabt.Term where

import Data.Foldable
import Data.Set (Set)
import Data.Set as Set
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe
import Data.Tuple
import Prelude hiding (zero)
import Zabt.Index
import Zabt.Nameless

-- | An abstract @'Term' v f@ is an abstract binding tree of the shape described
-- by the pattern functor @f@ augmented with variables named by @v@. Equality is
-- alpha-equivalence. In particular, @'Term' v f@ is (morally) equivalent to the
-- fixed-point of the pattern-algebra 'Zabt.View.View' respecting the binding
-- properties of 'Zabt.View.VAbs' and 'Zabt.View.VVar'.
data Term v f = Term (Set v) (Nameless v f (Term v f))

derive instance eqTerm :: (Eq v, Eq (f (Term v f))) => Eq (Term v f)
derive instance ordTerm :: (Ord v, Ord (f (Term v f))) => Ord (Term v f)

instance showTerm :: (Show v, Show (Nameless v f (Term v f))) => Show (Term v f) where
  show (Term v f) = "(Term " <> show v <> " " <> show f <> ")"

-- | Returns the free variables used within a given @Term@.
--
-- NOTE: We have to define a new function in order to not accidentally break
-- encapsulation. Just exporting @free@ direction would allow uses to manipulate
-- the Term value and break invariants (!).
freeVars :: ∀ v f. Term v f -> Set v
freeVars (Term f _) = f

project :: ∀ v f. Term v f -> Nameless v f (Term v f)
project (Term _ p) = p

embed :: ∀ v f. (Ord v, Foldable f) => Nameless v f (Term v f) -> Term v f
embed nls = case nls of
  Free v -> Term (Set.singleton v) nls
  Bound i -> Term Set.empty nls
  Pattern f -> Term (foldMap freeVars f) nls
  -- NOTE that embedding Abstraction here doesn't affect the free variables! That
  -- only occurs when embedding a View
  Abstraction (Scope v nls') -> Term (freeVars nls') nls

var :: ∀ v f. (Foldable f, Ord v) => v -> Term v f
var v = embed (Free v)

abstract :: ∀ v f. (Foldable f, Functor f, Ord v) => v -> Term v f -> Term v f
abstract name = go zero where
  go idx t
    | not (Set.member name (freeVars t)) = t
    | otherwise =
        Term (Set.delete name (freeVars t)) $ case project t of
          Free v
            | v == name -> Bound idx
            | otherwise -> Free v
          Bound _ -> project t
          Abstraction (Scope v t') -> Abstraction (Scope v (go (next idx) t'))
          Pattern f -> Pattern (map (go idx) f)

substitute :: ∀ v f. (Functor f, Foldable f, Ord v) => v -> (Term v f -> Term v f)
substitute = substitute' <<< var

substitute' :: ∀ v f. (Functor f, Foldable f, Ord v) => Term v f -> (Term v f -> Term v f)
substitute' value = go zero where
  go idx t =
    case project t of
      Free v -> t
      Bound idx'
        | idx == idx' -> value
        | otherwise -> t
      Abstraction (Scope v t') -> embed (Abstraction (Scope v (go (next idx) t')))
      Pattern f -> embed (Pattern (map (go idx) f))

-- | Substitute some free variables.
subst :: ∀ v f. (Functor f, Foldable f, Ord v) => (v -> Maybe (Term v f)) -> (Term v f -> Term v f)
subst ss = go where
  go t = case project t of
    Free v -> fromMaybe t (ss v)
    Bound _ -> t
    Abstraction (Scope v t') -> embed (Abstraction (Scope v (go t')))
    Pattern f -> embed (Pattern (map go f))

-- | Substitute some free variables from a finite map.
substMap :: ∀ v f. (Functor f, Foldable f, Ord v) => Map v (Term v f) -> (Term v f -> Term v f)
substMap ss = subst (_ `Map.lookup` ss)

-- | Substitute just one free variable.
subst1 :: ∀ v f. (Functor f, Foldable f, Ord v) => Tuple v (Term v f) -> (Term v f -> Term v f)
subst1 (Tuple v value) = subst (\v' -> if v == v' then Just value else Nothing)
