module Helper (shuffle, takeOutAt) where
import System.Random

shuffle :: RandomGen g => g -> [a] -> ([a], g)
shuffle g l = shuffler g l (length l)

shuffler :: RandomGen g => g -> [a] -> Int -> ([a], g)
shuffler g _ 0 = ([], g)
shuffler g [] _ = ([], g)
shuffler g ls n = let
    (i, g') = randomR (0,n-1) g
    (rest, now) = takeOutAt i ls
    (shuffled, g'') = shuffler g' rest (n-1)
    in
        (now:shuffled, g'')

takeOutAt :: Int -> [a] -> ([a], a)
takeOutAt n ls = let (pre, m:post) = splitAt n ls
                     in
                         (pre++post, m)
