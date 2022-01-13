module Util where

import Data.List (find)
import Data.Maybe (isJust, fromJust)

firstOne :: Foldable t => t (Maybe a) -> Maybe a
firstOne x = case find isJust x of
               Nothing -> Nothing
               Just x  -> Just $ fromJust x

unimplemented :: a
unimplemented = error "Not implemented -- this is not a bug, this program is currently incomplete. \
                      \If you're seeing this message, please contact with the developers."
