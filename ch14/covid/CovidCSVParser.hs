{-# LANGUAGE RecordWildCards #-}

module CovidCSVParser where

import Data.Attoparsec.ByteString.Char8 as A
import Data.Time (Day, fromGregorian)
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.Text.Encoding as T
import Data.Either

import CovidData

data CountryCodeWithRest = CountryCodeWithRest
  {code :: ByteString,
   rest :: ByteString}

-- Parser combinators

notEOL :: Char -> Bool
notEOL c = c /= '\n' && c /= '\r'

fieldSep :: Char
fieldSep = ','

field :: Parser ByteString
field = A.takeTill (\c -> c == fieldSep) <* char fieldSep

textField :: Parser Text
textField = T.decodeUtf8 <$> field

skipField :: Parser ()
skipField = skipWhile (\c -> c /= ',') <* char ','

skipLine :: Parser ()
skipLine = skipWhile notEOL <* endOfLine

countryCode :: Parser ByteString
countryCode = A.take 3 <* char ','

countryCodeWithRest :: Parser CountryCodeWithRest
countryCodeWithRest = CountryCodeWithRest <$> countryCode <*> A.takeWhile notEOL <* endOfLine

notCountry :: Parser ()
notCountry = skipWhile notEOL <* endOfLine

countryCodeWithRestOrSkip :: Parser (Maybe CountryCodeWithRest)
countryCodeWithRestOrSkip =
  choice [Just <$> countryCodeWithRest, const Nothing <$> notCountry]

fullCountryData :: ByteString -> Parser CountryData
fullCountryData code =
  CountryData <$> pure code
              <*> textField
              <*> textField
              <*> pure 0
              <*> pure 0
              <*> dayInfo
              <* count 12 skipField
              <*> statInfo

dayInfoOnly :: Parser [(Day, DayInfo)]
dayInfoOnly = count 2 skipField
              *> dayInfo

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

statInfo :: Parser CountryStat
statInfo = CountryStat <$> decimal <* skipField
              <*> (option Nothing $ Just <$> double)

-- Parsers

parseFullCountryData :: CountryCodeWithRest -> Maybe CountryData
parseFullCountryData CountryCodeWithRest {..} =
  either (const Nothing) Just $ parseOnly (fullCountryData code) rest

parseDayInfo :: CountryCodeWithRest -> [(Day, DayInfo)]
parseDayInfo CountryCodeWithRest {..} = fromRight [] $ parseOnly dayInfoOnly rest
