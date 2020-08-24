{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Hasql.Session (Session)
import Hasql.Statement (Statement(..))
import qualified Hasql.Session as Session
import qualified Hasql.Connection as Connection
import qualified Hasql.TH as TH

import Data.Int

countActorsStmt :: Statement () Int64
countActorsStmt = [TH.singletonStatement| select count(*) :: int8 from actor |]

countActors :: Session Int64
countActors = Session.statement () countActorsStmt

main :: IO ()
main = do
    Right conn <- Connection.acquire connectionSettings
    Session.run countActors conn >>= print
    putStrLn "OK"
  where
    connectionSettings =
      Connection.settings "localhost" 5432 "" "" "sakila_films"
