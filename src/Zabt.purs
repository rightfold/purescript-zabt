-- | Abstract binding trees with a nameless internal representation.
module Zabt
  ( module Zabt.Freshen
  , module Zabt.Internal.Term
  , module Zabt.View
  ) where

import Zabt.Freshen
import Zabt.Internal.Term
import Zabt.View

{- $intro

  Abstract binding trees take the form @'Term' v f a@, or, more commonly, @'Flat' v f@. These
  types are abstract—you will never construct or analyze them directly.

 -}

{- $frees

  Abstract binding trees take the form @'Term' v f a@, or, more commonly, @'Flat' v f@. These
  types are abstract---you will never construct or analyze them directly.

 -}
