module OneThree where
import Data.Char (isAlpha)

sourceText = unlines [
            "Now I need a drink, alcoholic of course,",
            "after the heavy lectures involving quantum mechanics."
            ]

wordLengths :: String -> [Int]
wordLengths = map (length . filter isAlpha) . words

-- |
-- >>> wordLengths sourceText
-- [3,1,4,1,5,9,2,6,5,3,5,8,9,7,9]
