module DataStructs.HashSet (
    (\\)
  , Hashable
  , module Data.HashSet
) where

import Data.Hashable
import Data.HashSet

(\\) :: (Eq a, Hashable a) => HashSet a -> HashSet a -> HashSet a
(\\) = difference
