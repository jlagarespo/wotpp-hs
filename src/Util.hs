module Util where

import Data.List (find)
import Data.Maybe (isJust, fromJust)

firstOne :: Foldable t => t (Maybe a) -> Maybe a
firstOne x = case find isJust x of
               Nothing -> Nothing
               Just x  -> Just $ fromJust x
