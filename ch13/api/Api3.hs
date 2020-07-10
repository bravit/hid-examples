{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- new GHC extensions
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Kind
import GHC.TypeLits
import Data.Proxy

import Control.Applicative ((<|>))

import Text.Read (readMaybe)

data Rating = Bad | Good | Great
  deriving Show

data ServiceStatus = Ok | Down
  deriving Show

data Get (a :: Type)

data Capture (a :: Type)

data a :<|> b = a :<|> b
infixr 8 :<|>

data (a :: k) :> (b :: Type)
infixr 9 :>

type BookID = Int

type BookInfoAPI = Get ServiceStatus
                   :<|> "title" :> Capture BookID :> Get String
                   :<|> "year" :> Capture BookID :> Get Int
                   :<|> "rating" :> Capture BookID :> Get Rating

type HandlerAction a = IO a

type family Server layout :: Type
type instance Server (Get a) = HandlerAction a
type instance Server (a :<|> b) = Server a :<|> Server b
type instance Server ((s :: Symbol) :> r) = Server r
type instance Server (Capture a :> r) = a -> Server r

impl1 :: Server BookInfoAPI
impl1 = pure Ok
       :<|> title
       :<|> year
       :<|> rating
  where
    title _ = pure "Haskell in Depth"
    year _ = pure 2020
    rating _ = pure Great

impl2 :: Server BookInfoAPI
impl2 = pure Down
        :<|> title
        :<|> year
        :<|> rating
  where
    notImplemented = ioError (userError "not implemented")
    title _ = notImplemented
    year _ = notImplemented
    rating _ = notImplemented


encode :: Show a => IO a -> IO String
encode m = show <$> m

type Request = [String]

class HasServer layout where
  route :: Proxy layout -> Server layout -> Request -> Maybe (IO String)

instance Show a => HasServer (Get a) where
  route :: Proxy (Get a)
        -> HandlerAction a -> Request -> Maybe (IO String)
  route _ handler [] = Just (encode $ handler)
  route _ _       _  = Nothing

instance {-# OVERLAPS #-} HasServer (Get String) where
  route :: Proxy (Get String)
        -> IO String -> Request -> Maybe (IO String)
  route _ handler [] = Just handler
  route _ _       _  = Nothing

instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  route :: Proxy (a :<|> b)
        -> (Server a :<|> Server b) -> Request -> Maybe (IO String)
  route _ (handlera :<|> handlerb) xs =
        route (Proxy :: Proxy a) handlera xs
    <|> route (Proxy :: Proxy b) handlerb xs

instance (KnownSymbol s, HasServer r) => HasServer ((s :: Symbol) :> r) where
  route :: Proxy (s :> r)
        -> Server r -> Request -> Maybe (IO String)
  route _ handler (x : xs)
    | symbolVal (Proxy :: Proxy s) == x = route (Proxy :: Proxy r) handler xs
  route _ _       _                     = Nothing

instance (Read a, HasServer r) => HasServer (Capture a :> r) where
  route :: Proxy (Capture a :> r)
        -> (a -> Server r) -> [String] -> Maybe (IO String)
  route _ handler (x : xs) = do
    a <- readMaybe x
    route (Proxy :: Proxy r) (handler a) xs
  route _ _       _        = Nothing

get :: HasServer layout => Proxy layout -> Server layout -> [String] -> IO String
get proxy handler request = case route proxy handler request of
  Nothing -> ioError (userError "404")
  Just m  -> m

check :: Server BookInfoAPI -> IO ()
check impl = do
  b <- get (Proxy :: Proxy BookInfoAPI) impl []
  answer <- get (Proxy :: Proxy BookInfoAPI) impl ["year", "7548"]
  putStrLn (if b == "Ok" && answer == "2020"
            then "OK"
            else "Wrong answer!")

main :: IO ()
main = check impl1
