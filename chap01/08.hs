module OneEight where
import Data.Char

cipher :: String -> String
cipher = let f c = if isLower c then (chr . (219 -) . ord) c
                                else c
            in
                map f

-- |
-- >>> cipher ['a'..'z'] == ['z', 'y' .. 'a']
-- True
-- >>> cipher . cipher $ "Hello,World. This is GOOD!"
-- "Hello,World. This is GOOD!"
