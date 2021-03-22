module DataStructs.HashMap (
    unionsWith
  , withoutKeys
  , Hashable
  , module Data.HashMap.Strict
) where

import Data.Hashable
import Data.HashMap.Strict

import qualified DataStructs.HashSet as S

unionsWith :: (Foldable t, Eq k, Hashable k) 
           => (a -> a -> a) -> t (HashMap k a) -> HashMap k a
unionsWith unifier = Prelude.foldr (unionWith unifier) empty

withoutKeys :: (Eq k, Hashable k)   
            => HashMap k a -> S.HashSet k -> HashMap k a
withoutKeys mp dropKeys = filterWithKey (\k _ -> not $ k `S.member` dropKeys) mp
