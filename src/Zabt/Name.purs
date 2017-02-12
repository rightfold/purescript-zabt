
-- | Example variable name class you may want to use.
module Zabt.Name where

import Data.String

import Zabt.Freshen

data Name = Name Int String

derive instance eqName :: Eq Name
derive instance ordName :: Ord Name

instance showName :: Show Name where
  show (Name n s)
    | n == 0 = '\'' : s
    | otherwise = '\'' : s ++ show n

instance freshenName :: Freshen Name where
  freshen (Name n s) = Name (n + 1) s
