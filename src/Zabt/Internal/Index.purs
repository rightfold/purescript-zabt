
-- | Newtype for de Bruijn indices.
module Zabt.Internal.Index where

import Zabt.Freshen

-- | A de Bruijn index.
newtype Index = Index Int

derive newtype instance eqIndex :: Eq Index
derive newtype instance ordIndex :: Ord Index

zero :: Index
zero = Index 0

next :: Index -> Index
next (Index i) = Index (i + 1)

instance showIndex :: Show Index where
  show (Index i) = "(Index " <> show i <> ")"

instance freshenIndex :: Freshen Index where
  freshen = next
