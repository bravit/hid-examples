{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

module ProcessRequest (processMany, processInteractively) where

import System.Locale.Read (TimeLocale, getCurrentLocale)
import Control.Exception.Safe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time
import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class

import Control.Concurrent
import Data.Semigroup ((<>)) -- required for GHC 8.2

import App
import Types
import GeoCoordsReq
import SunTimes
import STExcept

parseRequestLine :: T.Text -> Either RequestError (T.Text, When)
parseRequestLine t = parse (split t)
  where
    split t = case T.breakOn "@" t of
                (addr, "") -> ("", addr)
                (day, addr) -> (T.strip day, T.strip $ T.tail addr)
    parse (_, "") = Left EmptyRequest
    parse ("", addr) = Right (addr, Now)
    parse (d, addr) =
      case parseTimeM False defaultTimeLocale "%Y-%m-%d" (T.unpack d) of
        Nothing -> Left (WrongDay d)
        Just d' -> Right (addr, On d')

formatResult :: T.Text -> SunTimes ZonedTime -> TimeLocale -> T.Text
formatResult req SunTimes {..} loc = mconcat [day, " @ ", req,
                                              "\n    ", sr,
                                              "\n    ", ss]
  where
    day = T.pack $ formatTime loc "%x" sunrise
    sr = T.pack $ formatTime loc "%X %Z" sunrise
    ss = T.pack $ formatTime loc "%X %Z" sunset

processRequest :: T.Text -> MyApp T.Text
processRequest t = processR (parseRequestLine (T.strip t))
  where
    processR (Left e) = throw (FormatError e)
    processR (Right (addr, day)) = do
      coords <- getCoords addr
      st <- getSunTimes coords day 
      loc <- liftIO getCurrentLocale
             `catchAny` (const $ pure defaultTimeLocale)
      pure $ formatResult addr st loc

processMany :: [T.Text] -> MyApp ()
processMany = mapM_ processRequestWrapper
  where
    processRequestWrapper r =
      unless ("#" `T.isPrefixOf` r)
             $ (processRequest r >>= liftIO .TIO.putStrLn) `catch` handler r
               `finally` delaySec 3
    delaySec sec = liftIO $ threadDelay (sec * 1000000)
    handler :: T.Text -> SunInfoException -> MyApp ()
    handler r e = liftIO $ TIO.putStrLn $ "Error in request '" <> r <> "': "
                         <> T.pack (show e)

processInteractively :: MyApp ()
processInteractively = action `catch` handler
  where
    action = do 
      liftIO $ TIO.putStrLn "Enter your request:"
      req <- liftIO $ TIO.getLine
      res <- processRequest req
      liftIO $ TIO.putStrLn res

    handler :: SunInfoException -> MyApp ()
    handler e@(ServiceAPIError _) = liftIO $ print e
    handler e@(NetworkError _) = liftIO $ print e
    handler e = do
      liftIO $ TIO.putStr
             $ "There was an error while processing your request: "
             <> T.pack (show e) <> "\nDo you want to try again (Y/N)?"
      yesno <- liftIO $ TIO.getLine
      when (yesno `elem` ["y", "Y", "yes"]) processInteractively
