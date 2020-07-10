import Text.Read (readMaybe)

data Rating = Bad | Good | Great
  deriving Show

data ServiceStatus = Ok | Down
  deriving Show

type BookID = Int
type HandlerAction a = IO a
type ReqHandler a = BookID -> HandlerAction a

data BookInfoAPIImpl = BookInfoAPIImpl {
                         root :: HandlerAction ServiceStatus,
                         title :: ReqHandler String,
                         year :: ReqHandler Int,
                         rating :: ReqHandler Rating }

impl1 :: BookInfoAPIImpl
impl1 = BookInfoAPIImpl {
    root = pure Ok,
    title = \_ -> pure "Haskell in Depth",
    year = \_ -> pure 2020,
    rating = \_ -> pure Great
  }

impl2 :: BookInfoAPIImpl
impl2 = BookInfoAPIImpl {
    root = pure Down,
    title = \_ -> notImplemented,
    year = \_ -> notImplemented,
    rating = \_ -> notImplemented
  }
  where
    notImplemented = ioError (userError "not implemented")

type Request = [String]

encode :: Show a => HandlerAction a -> IO String
encode m = show <$> m

route :: BookInfoAPIImpl -> Request -> Maybe (IO String)
route impl [] = pure $ encode $ root impl
route impl [op, bid'] = do
  bid <- readMaybe bid'
  case op of
    "title" -> pure $ title impl bid
    "year" -> pure $ encode $ year impl bid
    "rating" -> pure $ encode $ rating impl bid
    _ -> Nothing
route _ _ = Nothing

get :: BookInfoAPIImpl -> Request -> IO String
get impl xs =
  case route impl xs of
    Just m -> m
    Nothing -> pure "Malformed request"

check :: BookInfoAPIImpl -> IO ()
check impl = do
  b <- get impl []
  answer <- get impl ["year", "7548"]
  putStrLn (if b == "Ok" && answer == "2020"
            then "OK"
            else "Wrong answer!")

main :: IO ()
main = check impl1
