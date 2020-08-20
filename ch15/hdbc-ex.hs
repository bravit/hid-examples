import Database.HDBC
import Database.HDBC.PostgreSQL


-- "host=localhost dbname=pagila user=testuser password=pass"

countActors :: Connection -> IO Int
countActors conn = do
  select <- prepare conn "SELECT count(*) FROM actor;"
  _ <- execute select []
  result <- fetchAllRows select
  case result of
    [[val]] -> pure $ fromSql val
    _ -> error "Unexpected result"

main :: IO ()
main = withPostgreSQL "host=localhost dbname=pagila" $ \conn -> do
  r <- countActors conn
  print r
