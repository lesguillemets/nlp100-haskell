module OneSeven where

f :: (Show a, Show b)  => a -> String -> b -> String
f x y z = concat [show x, "時の", y, "は", show z]
-- |
-- >>> putStr $ f 12 "気温" 22.4
-- 12時の気温は22.4

