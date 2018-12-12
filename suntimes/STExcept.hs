module STExcept where

import qualified Data.Text as T
import Control.Exception.Safe
import Network.HTTP.Req
import qualified Network.HTTP.Client as NC

import Types

data RequestError = EmptyRequest | WrongDay T.Text
  deriving Show

data SunInfoException = UnknownLocation T.Text
                      | UnknownTime GeoCoords
                      | FormatError RequestError
                      | ServiceAPIError String
                      | NetworkError SomeException

instance Show SunInfoException where
  show (UnknownLocation loc) = "Failed while determining coordinates"
  show (UnknownTime loc) = "Failed while determining sunrise/sunset times"
  show (FormatError er) = show er
  show (ServiceAPIError _) = "Error while communicating with external services"
  show (NetworkError _) = "Network communication error"
  

instance Exception SunInfoException

rethrowReqException :: MonadThrow m => HttpException -> m a
rethrowReqException (JsonHttpException s) = throw (ServiceAPIError s)
rethrowReqException (VanillaHttpException (
                        NC.HttpExceptionRequest _
                          (NC.StatusCodeException resp _ ))) =
  throw (ServiceAPIError $ show $ NC.responseStatus resp)
rethrowReqException (VanillaHttpException e) = throw (NetworkError $ toException e)
