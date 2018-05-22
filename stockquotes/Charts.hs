module Charts (plotChart) where

import Data.Foldable (traverse_, toList)
import Data.Time
import Data.Default.Class
import Graphics.Rendering.Chart.Easy (plot, line, (.=), layout_title)
import Graphics.Rendering.Chart.Backend.Diagrams (toFile,
                                               FileOptions(..),
                                               FileFormat(..))
import QuoteData

plotChart :: (Functor t, Foldable t) =>
             String -> t QuoteData -> [QField] -> FilePath -> IO ()
plotChart title quotes qfs fname =
  toFile def {_fo_format=SVG} fname $ do
    layout_title .= title
    traverse_ plotLine qfs
  where
    plotLine qf = plot $ line (show qf) [toList $ fmap (qf2pd qf) quotes]
    qf2pd qf q = (LocalTime (day q) midday,
                  realToFrac $ field2fun qf q :: Double)
