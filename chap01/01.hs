module OneOne where

-- perhaps we want something more generic?
takeOdds :: [a] -> [a]
takeOdds [] = []
takeOdds [x] = [x]
takeOdds (x:_:xs) = x: takeOdds xs
-- |
-- >>> putStr $ takeOdds "パタトクカシー"
-- パトカー
