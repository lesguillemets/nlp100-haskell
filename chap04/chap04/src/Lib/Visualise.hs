module Lib.Visualise where

import Lib.Morph
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

barChart :: (PrettyPrintable a , BarsPlotValue n)=> FilePath -> [(a,n)] -> IO ()
barChart fName elems = toFile option fName $ do
    layout_all_font_styles . font_name .= "IPAexGothic"
    plot . fmap plotBars $ bars names freqs
    where
        names = map (pp . fst) elems
        freqs = zip (map PlotIndex [0..]) . map (return . snd) $ elems

option :: FileOptions
option = FileOptions (800, 600) SVG
