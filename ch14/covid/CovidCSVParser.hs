{-# LANGUAGE RecordWildCards #-}

module CovidCSVParser where

import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString.Char8 as A
import Data.Time (Day, fromGregorian)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Either (fromRight)
import Control.Applicative ((<|>))

import CovidData

data CountryCodeWithRest = CountryCodeWithRest {
    code :: ByteString
  , rest :: ByteString
  }

-- Parser combinators

notEOL :: Char -> Bool
notEOL c = c /= '\n' && c /= '\r'

countryCode :: Parser ByteString
countryCode = A.take 3 <* char ','

countryCodeWithRest :: Parser CountryCodeWithRest
countryCodeWithRest =
  CountryCodeWithRest <$> countryCode
                      <*> A.takeWhile notEOL
                      <* endOfLine

skipLine :: Parser ()
skipLine = skipWhile notEOL <* endOfLine

countryCodeWithRestOrSkip :: Parser (Maybe CountryCodeWithRest)
countryCodeWithRestOrSkip =
  Just <$> countryCodeWithRest <|> const Nothing <$> skipLine

field :: Parser ByteString
field = takeTill (\c -> c == ',') <* char ','

textField :: Parser Text
textField = decodeUtf8 <$> field

skipField :: Parser ()
skipField = skipWhile (\c -> c /= ',') <* char ','

intField :: Parser Int
intField = (decimal <|> pure 0) <* skipField

fullCountryData :: ByteString -> Parser CountryData
fullCountryData code =
  CountryData <$> pure code
              <*> textField -- continent
              <*> textField -- country name
              <*> pure 0 -- current total cases
              <*> pure 0 -- current total deaths
              <*> dayInfo
              <* count 14 skipField
              <*> statInfo

dayInfoOnly :: Parser [(Day, DayInfo)]
dayInfoOnly = count 2 skipField *> dayInfo

dayInfo :: Parser [(Day, DayInfo)]
dayInfo = (\a b -> [(a,b)]) <$> dayParser <*> dayInfoParser
  where
    dayParser =
      fromGregorian <$> decimal <* char '-'
                    <*> decimal <* char '-'
                    <*> decimal <* char ','

    dayInfoParser = DayInfo <$> dayCasesParser <*> dayDeathsParser
    dayCasesParser = DayCases <$> intField <*> intField
    dayDeathsParser = DayDeaths <$> intField <*> intField

statInfo :: Parser CountryStat
statInfo = CountryStat <$> intField -- population
                       <*> (option Nothing $ Just <$> double) -- density

-- Parsers

parseFullCountryData :: CountryCodeWithRest -> Maybe CountryData
parseFullCountryData CountryCodeWithRest {..} =
  either (const Nothing) Just $ parseOnly (fullCountryData code) rest

parseDayInfo :: CountryCodeWithRest -> [(Day, DayInfo)]
parseDayInfo CountryCodeWithRest {..} =
  fromRight [] $ parseOnly dayInfoOnly rest
