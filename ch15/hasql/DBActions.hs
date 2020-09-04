module DBActions where

import Hasql.Connection (Connection)
import qualified Hasql.Session as Session

import Data.Int
import Data.Vector (Vector)
import Data.Text (Text)

import FilmInfo.Data
import qualified Sessions as Ses

handleErrors :: IO (Either Session.QueryError a) -> IO a
handleErrors m = do
  r <- m
  case r of
    Right v -> pure v
    Left err -> error $ show err

allFilms :: Connection -> IO (Vector FilmInfo)
allFilms conn = handleErrors $ Session.run Ses.allFilms conn

totalFilmsNumber :: Connection -> IO Int64
totalFilmsNumber conn = handleErrors $ Session.run Ses.countFilms conn

findFilm :: Connection -> Text -> IO (Maybe FilmInfo)
findFilm conn ttl = handleErrors $ Session.run (Ses.findFilm ttl) conn

filmsLonger :: Connection -> FilmLength -> IO (Vector FilmInfo)
filmsLonger conn len = handleErrors $ Session.run (Ses.filmsLonger len) conn

filmsCategories :: Connection -> [Text] -> IO [FilmCategories]
filmsCategories conn films =
  handleErrors $ Session.run (Ses.filmsCategories films) conn

setRating :: Connection -> Rating -> Text -> IO Int64
setRating conn newRating film =
  handleErrors $ Session.run (Ses.setRating newRating film) conn

assignCategory :: Connection -> Text -> Text -> IO Int64
assignCategory conn catName filmTitle =
  handleErrors $ Session.run (Ses.assignCategory catName filmTitle) conn

unassignCategory :: Connection -> Text -> Text -> IO Int64
unassignCategory conn catName filmTitle =
  handleErrors $ Session.run (Ses.unassignCategory catName filmTitle) conn

printAllFilms :: Connection -> IO ()
printAllFilms conn = handleErrors $ Session.run (Ses.processAllFilms printFilm) conn
