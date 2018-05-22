{-# LANGUAGE
     OverloadedStrings,
     RecordWildCards
#-}

module HtmlReport (htmlReport) where

import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.Semigroup ((<>))
import qualified Data.ByteString.Lazy as BL
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8

import QuoteData

htmlReport :: (Functor t, Foldable t) =>
              String -> t QuoteData -> [FilePath] -> BL.ByteString
htmlReport title quotes images = renderHtml $ docTypeHtml $ do
     H.head $ do
       H.title $ string title
       H.style style
     body $ do
       when (not $ null images) $ do
         h1 "Diagrams"
         traverse_ ((img!).src.toValue) images
       h1 "Stock Quotes Data"
       table ! A.style "" $ do
         thead $ traverse_ th tcolumns
         tbody $ traverse_ quoteData2TR quotes
  where
    style = "table {border-collapse: collapse}" <>
            "td, th {border: 1px solid black}"
    tcolumns = ["Day", "Close", "Volume", "Open", "High", "Low"]
    quoteData2TR :: QuoteData -> Html
    quoteData2TR QuoteData {..} = tr $ do
        td $ string $ show day
        traverse_ (td.string.show) [close, volume, open, high, low] 
