{-# LANGUAGE DeriveAnyClass #-}

module STExcept where

import Data.Text (Text)
import Control.Monad.Catch
import Network.HTTP.Req
import qualified Network.HTTP.Client as NC

import Types

data RequestError = EmptyRequest | WrongDay Text
  deriving Show

data SunInfoException = UnknownLocation Text
                      | UnknownTime GeoCoords
                      | FormatError RequestError
                      | ServiceAPIError String
                      | NetworkError SomeException
                      | ConfigError
   deriving Exception

instance Show SunInfoException where
  show (UnknownLocation _) = "Failed while determining coordinates"
  show (UnknownTime _) = "Failed while determining sunrise/sunset times"
  show (FormatError er) = show er
  show (ServiceAPIError _) = "Error while communicating with external services"
  show (NetworkError _) = "Network communication error"
  show ConfigError = "Error parsing configuration file"

rethrowReqException :: MonadThrow m => HttpException -> m a
rethrowReqException (JsonHttpException s) = throwM (ServiceAPIError s)
rethrowReqException (VanillaHttpException (
                        NC.HttpExceptionRequest _
                          (NC.StatusCodeException resp _ ))) =
  throwM (ServiceAPIError $ show $ NC.responseStatus resp)
rethrowReqException (VanillaHttpException e) = throwM (NetworkError $ toException e)
