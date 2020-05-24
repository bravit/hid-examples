{-# LANGUAGE RecordWildCards #-}

module Charts (plotChart) where

import Data.Foldable (toList)
import Graphics.Rendering.Chart.Easy hiding (close, label, bars)
import Graphics.Rendering.Chart.Backend.Diagrams

import QuoteData

plotChart :: Foldable t =>
             String
             -> t QuoteData
             -> FilePath
             -> IO ()
plotChart title quotes fname = do
    _ <- renderableToFile fileOptions fname (toRenderable chart)
    pure ()
  where
    fileOptions = FileOptions (800, 600) SVG loadSansSerifFonts

    (candles, closings, volumes) = unzip3 $
      [ (Candle day low open 0 close high,
        (day, close),
        (day, [volume])) | QuoteData {..} <- toList quotes ]

    chart = slayouts_layouts .~
        [ StackedLayout $ candlesLayout,
          StackedLayout $ volumesLayout
        ]
      $ def

    candlesLayout =
       layout_title .~ title
     $ layout_plots .~ [ toPlot $ qline "Close" closings green,
                         toPlot $ candle "Candle" candles cyan ]
     $ def

    volumesLayout =
       layout_plots .~ [ plotBars $ bars "Volume" volumes gray ]
     $ def

    candle label values color =
       plot_candle_line_style  .~ lineStyle 1 gray
     $ plot_candle_fill .~ True
     $ plot_candle_rise_fill_style .~ fillStyle white
     $ plot_candle_fall_fill_style .~ fillStyle color
     $ plot_candle_tick_length .~ 0
     $ plot_candle_width .~ 3
     $ plot_candle_values .~ values
     $ plot_candle_title .~ label
     $ def

    qline label values color =
       plot_lines_style .~ lineStyle 1 color
     $ plot_lines_values .~ [values]
     $ plot_lines_title  .~ label
     $ def

    bars label values color =
       plot_bars_titles .~ [label]
     $ plot_bars_values .~ values
     $ plot_bars_item_styles .~ [(fillStyle color, Nothing)]
     $ def

    fillStyle color = solidFillStyle (opaque color)

    lineStyle n color =
       line_width .~ n
     $ line_color .~ opaque color
     $ def
