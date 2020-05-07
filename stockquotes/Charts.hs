{-# LANGUAGE RecordWildCards #-}
module Charts (plotChart) where

import Data.Foldable (traverse_, toList)

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Data.Time (Day)
import QuoteData as Q


plotChart :: (Functor t, Foldable t) =>
             String -> t QuoteData -> [QField] -> FilePath -> IO ()
plotChart title quotes qfs fname = toFile fileOptions fname $ do
    layoutlr_title .= title
    layoutlr_left_axis . laxis_override .= axisGridHide
    layoutlr_right_axis . laxis_override .= axisGridHide
    plotRight $ qline Volume blue
    plotLeft $ qline Q.Close green
    plotLeft (candle "Candle" magenta $ toList quotes)
  where
    fileOptions = FileOptions (800, 600) SVG loadSansSerifFonts
    candle label color quotes = liftEC $ do
      plot_candle_line_style  .= lineStyle 1 color
      plot_candle_fill .= True
      plot_candle_rise_fill_style .= solidFillStyle (opaque white)
      plot_candle_fall_fill_style .= solidFillStyle (opaque color)
      plot_candle_tick_length .= 0
      plot_candle_width .= 2
      plot_candle_values .= map quote2Candle quotes
      plot_candle_title .= label
    qline qf color = liftEC $ do
      plot_lines_style .= lineStyle 1 color
      plot_lines_values .= [toList $ fmap (qf2pd qf) quotes]
      plot_lines_title  .= show qf
    quote2Candle :: QuoteData -> Candle Day Double
    quote2Candle QuoteData {..} =
      Candle day
             (realToFrac low)
             (realToFrac open)
             0
             (realToFrac close)
             (realToFrac high)
    lineStyle n color = line_width .~ n
                         $ line_color .~ opaque color
                         $ def
    qf2pd qf q = (day q,
                  realToFrac $ field2fun qf q :: Double)
