module Util where

onlyRights :: [Either l r] -> Either l [r]
onlyRights (Left x:_) = Left x
onlyRights (Right x:xs) = (x:) <$> onlyRights xs
onlyRights [] = pure []
