{-# LANGUAGE RecordWildCards #-}

module ProcessRequest (processMany, processInteractively) where

import System.Locale.Read
import Control.Exception.Safe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time
import Control.Monad
import Control.Concurrent
import Data.Semigroup ((<>)) -- required for GHC 8.2

import Types
import GeoCoordsReq
import SunTimes
import STExcept

processRequest :: T.Text -> WebAPIAuth -> IO T.Text
processRequest t wauth = processR (parseRequestLine (T.strip t))
  where
    processR (Left e) = throw (FormatError e)
    processR (Right (addr, day)) = do
      coords <- getCoords addr wauth
      st <- getSunTimes coords day wauth 
      loc <- getCurrentLocale `catchAny` (\_ -> pure defaultTimeLocale)
      pure $ formatResult addr st loc

parseRequestLine :: T.Text -> Either RequestFormatError (T.Text, When)
parseRequestLine t = parse (split t)
  where
    split t = case T.breakOn "@" t of
                (addr, "") -> ("", addr)
                (day, addr) -> (T.strip day, T.strip $ T.tail addr)
    parse (_, "") = Left EmptyRequest
    parse ("", addr) = Right (addr, Now)
    parse (d, addr) =
      case parseTimeM False defaultTimeLocale "%Y-%m-%d" (T.unpack d) of
        Nothing -> Left (IncorrectDayFormat d)
        Just d' -> Right (addr, On d')

formatResult :: T.Text -> SunTimes ZonedTime -> TimeLocale -> T.Text
formatResult req SunTimes {..} loc = mconcat [day, " @ ", req,
                                              "\n    ", sr,
                                              "\n    ", ss]
  where
    day = T.pack $ formatTime loc "%x" sunrise
    sr = T.pack $ formatTime loc "%X %Z" sunrise
    ss = T.pack $ formatTime loc "%X %Z" sunset

processMany :: WebAPIAuth -> [T.Text] -> IO ()
processMany wauth = mapM_ processRequestWrapper
  where
    processRequestWrapper r =
      unless ("#" `T.isPrefixOf` r)
             ((processRequest r wauth `catch` handler r >>= TIO.putStrLn)
              `finally` delaySec 3)
    delaySec sec = threadDelay (sec * 1000000)
    handler _ (NetworkError e) = throw (NetworkError e)
    handler r e = pure $ "Error in request '" <> r <> "': "
                         <> T.pack (show e)

processInteractively :: WebAPIAuth -> IO ()
processInteractively wauth = action `catch` handler
  where
    action = do 
      TIO.putStrLn "Enter your request:"
      req <- TIO.getLine
      res <- processRequest req wauth
      TIO.putStrLn res
    handler :: SunInfoException -> IO ()
    handler e = do
      TIO.putStr "There was an error while processing your request: "
      TIO.putStrLn $ T.pack $ show e
      TIO.putStrLn "Do you want to try again (Y/N)?"
      yes <- TIO.getLine
      when (yes `elem` ["y", "Y", "yes"]) (processInteractively wauth)
