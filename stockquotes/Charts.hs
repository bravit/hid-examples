{-# LANGUAGE RecordWildCards #-}
module Charts (plotChart) where

import Data.Foldable (toList)
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import QuoteData


plotChart :: (Foldable t, Functor t) =>
             String -> t QuoteData -> FilePath -> IO ()
plotChart title quotes fname = toFile fileOptions fname $ do
    layoutlr_title .= title
    layoutlr_left_axis . laxis_override .= axisGridHide
    layoutlr_right_axis . laxis_override .= axisGridHide
    plotRight $ qline "Volume" vols blue
    plotLeft $ qline "Close" clos green
    plotLeft $ candle "Candle" candles magenta
  where
    (candles, vols, clos) = unzip3 $
      [ (Candle day low open 0 close high,
        (day, volume),
        (day, close)) | QuoteData {..} <- toList quotes ]
    fileOptions = FileOptions (800, 600) SVG loadSansSerifFonts
    candle label values color = liftEC $ do
      plot_candle_line_style  .= lineStyle 1 color
      plot_candle_fill .= True
      plot_candle_rise_fill_style .= solidFillStyle (opaque white)
      plot_candle_fall_fill_style .= solidFillStyle (opaque color)
      plot_candle_tick_length .= 0
      plot_candle_width .= 2
      plot_candle_values .= values
      plot_candle_title .= label
    qline label values color = liftEC $ do
      plot_lines_style .= lineStyle 1 color
      plot_lines_values .= [values]
      plot_lines_title  .= label
    lineStyle n color = line_width .~ n
                         $ line_color .~ opaque color
                         $ def
