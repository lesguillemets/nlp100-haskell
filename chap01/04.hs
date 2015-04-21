module OneFour where
import Data.Map hiding (map)
import GHC.Exts (sortWith)
sourceText = unwords [
    "Hi He Lied Because Boron Could Not Oxidize Fluorine.",
    "New Nations Might Also Sign Peace Security Clause. Arthur King Can."
    ]

oneCharIndices :: [Int]
oneCharIndices = [1,5,6,7,8,9,15,16,19]

chemicalSymbols :: Map String Int
chemicalSymbols = fromList . zipWith f [1..] . words $ sourceText
    where
        f n name = if n `elem` oneCharIndices
                         then (take 1 name, n)
                         else (take 2 name, n)
-- |
-- >>> :{
--  let elements = [
--      "H","He","Li","Be","B","C","N","O","F","Ne",
--      "Na","Mi","Al","Si","P","S","Cl","Ar","K","Ca"
--      ]
-- :}
--
-- >>> map (chemicalSymbols !) elements == [1..20]
-- True
