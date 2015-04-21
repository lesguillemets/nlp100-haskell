module OneTwo where

-- fmm.. I feel I'm missing something.

joinMix :: [a] -> [a] -> [a]
joinMix [] ys = ys
joinMix xs [] = xs
joinMix (x:xs) (y:ys) = x:y:joinMix xs ys

-- |
-- >>> putStr $ joinMix "パトカー" "タクシー"
-- パタトクカシーー
