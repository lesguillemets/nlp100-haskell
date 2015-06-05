import qualified Data.ByteString.Char8 as BC
import Helper (mainWith)

expandTab :: BC.ByteString -> BC.ByteString
expandTab = BC.map (\c -> if c == '\t' then ' ' else c)

main = mainWith expandTab
