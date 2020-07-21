module CovidCSVParser where

import Data.Attoparsec.ByteString.Char8 as A
import Data.Time (Day, fromGregorian)
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.Text.Encoding as T

import CovidData

countryCodeWithRest :: Parser (ByteString, ByteString)
countryCodeWithRest = (,) <$> field
                          <*> A.takeWhile notEOL
                          <* endOfLine

fullCountryData :: ByteString -> Parser CountryData
fullCountryData code =
  CountryData <$> pure code
              <*> textField
              <*> textField
              <*> dayInfo
              <* count 12 skipField
              <*> statInfo

dayInfoOnly :: Parser [(Day, DayInfo)]
dayInfoOnly = count 2 skipField
              *> dayInfo

notSep c = c /= ',' && notEOL c
notEOL c = c /= '\n' && c /= '\r'

field = A.takeWhile (\c -> c /= ',') <* char ','
textField :: Parser Text
textField = T.decodeUtf8 <$> field
skipField = skipWhile (\c -> c /= ',') <* char ','

dayInfo :: Parser [(Day, DayInfo)]
dayInfo = (\a b -> [(a,b)]) <$> dayParser <*> dayInfoParser
  where
    dayParser =
      fromGregorian <$> decimal <* char '-'
                    <*> decimal <* char '-'
                    <*> decimal <* char ','

    dayInfoParser = DayInfo <$> dayCasesParser <*> dayDeathsParser

    dayCasesParser =
      DayCases <$> decimal <* skipField
               <*> decimal <* skipField

    dayDeathsParser =
      DayDeaths <$> decimal <* skipField
                <*> A.decimal <* skipField

statInfo :: Parser (Maybe CountryStat)
statInfo = option Nothing $ Just <$> (CountryStat <$> decimal <* skipField
              <*> double)
