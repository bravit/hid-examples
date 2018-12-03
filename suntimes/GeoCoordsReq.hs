module GeoCoordsReq (getCoords) where

import Network.HTTP.Req
import Data.Default
import Control.Exception.Safe
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import Types
import STExcept

getCoords :: Address -> WebAPIAuth -> IO GeoCoords
getCoords addr wauth = do
    res <- runReq def request `catch` rethrowReqException
    case res of
      [] -> throw (UnknownLocation addr)
      (coords:_) -> pure coords
  where
    request = responseBody <$>
              req GET ep NoReqBody jsonResponse reqParams
    ep = https "nominatim.openstreetmap.org" /:"search"
    reqParams =
      mconcat [
        "q" =: addr
      , "format" =: ("json" :: T.Text)
      , "limit" =: (1 :: Int)
      , "email" =: email wauth
      , header "User-Agent"
               (encodeUtf8 $ agent wauth)
      ]
