module Lib.Visualise where

import Lib.Morph

import Control.Arrow

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

barChart :: (PrettyPrintable a , BarsPlotValue n)=> FilePath -> [(a,n)] -> IO ()
barChart fName elems = toFile option fName $ do
    layout_all_font_styles . font_name .= "IPAexGothic"
    layout_x_axis . laxis_generate .= xAxis
    plot . fmap plotBars . bars ["Frequency"] $ results
    where
        xAxis = autoIndexAxis $ map (pp . fst) elems
        results = addIndexes $ map (return . snd) elems

option :: FileOptions
option = FileOptions (800, 600) SVG

histgram :: FilePath -> [Double] -> IO ()
histgram fName freqs = toFile option fName $ do
    layout_all_font_styles . font_name .= "IPAexGothic"
    plot . fmap histToPlot . liftEC $ do
        plot_hist_values .= freqs
        plot_hist_norm_func .= const id

zipF :: FilePath -> [(a, Int)] -> IO ()
zipF fName elems = toFile option fName $
    plot $ line "zipF" [map (takeLog *** takeLog)  . zip [1..] . map snd $ elems]
    where
        takeLog :: Int -> Double
        takeLog = log . fromIntegral
