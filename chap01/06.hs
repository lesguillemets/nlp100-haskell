module OneSix where
import Data.Set as S hiding (map)
-- fm, looks too specific
bigrams :: String -> Set String
bigrams source = fromList . map (\(x,y) -> [x,y]) $ zip source (tail source)

bigramx :: Set String
bigramx = bigrams "paraparaparadise"

bigramy :: Set String
bigramy = bigrams "paragraph"

-- |
-- >>> bigramx `union` bigramy
-- fromList ["ad","ag","ap","ar","di","gr","is","pa","ph","ra","se"]
-- >>> bigramx `intersection` bigramy
-- fromList ["ap","ar","pa","ra"]
-- >>> bigramx \\ bigramy
-- fromList ["ad","di","is","se"]
-- >>> "se" `member` bigramx
-- True
-- >>> "se" `member` bigramy
-- False
