{-# LANGUAGE RecordWildCards #-}
module Charts (plotChart) where

import Data.Foldable (toList)
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import QuoteData

plotChart :: (Foldable t, Functor t) =>
             String -> t QuoteData -> FilePath -> IO ()
plotChart title quotes fname = do
    renderableToFile fileOptions fname chart
    pure ()
  where
    chart = toRenderable $
      slayouts_layouts .~
        [ StackedLayout $ candlesL,
          StackedLayout $ volumesL
        ]
      $ slayouts_compress_legend .~ True
      $ def

    (candles, closings, volumes) = unzip3 $
      [ (Candle day low open 0 close high,
        (day, close),
        (day, [volume])) | QuoteData {..} <- toList quotes ]

    fileOptions = FileOptions (1200, 700) SVG loadSansSerifFonts

    candlesL = candlesLayout title candles closings
    volumesL = volumesLayout volumes

    candlesLayout title candles closings =
       layout_title .~ title
     $ layout_plots .~ [ toPlot $ qline "Close" closings gray,
                         toPlot $ candle "Candle" candles gray ]
     $ def

    volumesLayout volumes =
       layout_plots .~ [ plotBars $ bars "Volumes" volumes gray ]
     $ def

    candle label values color =
       plot_candle_line_style  .~ lineStyle 1 gray
     $ plot_candle_fill .~ True
     $ plot_candle_rise_fill_style .~ solidFillStyle (opaque white)
     $ plot_candle_fall_fill_style .~ solidFillStyle (opaque color)
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
     $ plot_bars_spacing .~ BarsFixGap 30 5
     $ plot_bars_item_styles .~
         [ (solidFillStyle (opaque color), Nothing) ]
     $ def

    lineStyle n color =
       line_width .~ n
     $ line_color .~ opaque color
     $ def
