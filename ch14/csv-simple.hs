import Data.ByteString (ByteString)
--import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Attoparsec.ByteString.Char8 as A
import Control.Applicative


import qualified Streaming.Prelude as S
import qualified Streaming.ByteString as BS
import Control.Monad.Trans.Resource
import Data.Attoparsec.ByteString.Streaming
import Data.Function ((&))
import Data.Functor (void)


field :: Parser ByteString
field = A.takeWhile (\c -> c /= ',' && c /= '\r' && c /= '\n')

textField :: Parser Text
textField = T.decodeUtf8 <$> field

record :: Parser [Text]
record = textField `sepBy1` (char ',')

endOfFile :: Parser ()
endOfFile = endOfInput <|> endOfLine *> endOfInput

file :: Parser [[Text]]
file =
  (:) <$> record
      <*> manyTill (endOfLine *> record)
                   endOfFile

main :: IO ()
-- main = B.readFile "data/quotes.csv" >>= print . parseOnly file
main = runResourceT $
           BS.readFile "data/quotes.csv"
         & parsed file
         & void
         & S.print
