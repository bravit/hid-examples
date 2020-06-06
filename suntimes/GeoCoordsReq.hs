module GeoCoordsReq (getCoords) where

import Network.HTTP.Req
import Control.Monad.Catch
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Control.Monad.Reader

import App
import Types
import STExcept

getCoords :: Address -> MyApp GeoCoords
getCoords addr = handle rethrowReqException $ do
    wauth <- ask
    let
      ep = https "nominatim.openstreetmap.org" /: "search"
      reqParams =
        mconcat [
          "q" =: addr
          , "format" =: ("json" :: T.Text)
          , "limit" =: (1 :: Int)
          , "email" =: email wauth
          , header "User-Agent" (encodeUtf8 $ agent wauth)
          ]
      request = req GET ep NoReqBody jsonResponse reqParams
    res <- liftIO $ responseBody <$> runReq defaultHttpConfig request
    case res of
      [] -> throwM (UnknownLocation addr)
      (coords:_) -> pure coords
