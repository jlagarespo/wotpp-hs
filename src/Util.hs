module Util where

import Data.Maybe (listToMaybe)

onlyRights :: [Either l r] -> Either l [r]
onlyRights (Left x:_) = Left x
onlyRights (Right x:xs) = (x:) <$> onlyRights xs
onlyRights [] = pure []

safeHead :: [a] -> Maybe a
safeHead = listToMaybe
