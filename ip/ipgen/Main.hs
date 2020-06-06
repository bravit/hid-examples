import qualified Hedgehog.Gen as Gen
import Options.Applicative as Opt
import Control.Monad.Catch
import System.Exit

import GenIP

data Params = Params
                Int
                FilePath

mkParams :: Opt.Parser Params
mkParams = Params
             <$> argument auto (metavar "SIZE" <> help "number of ranges")
             <*> argument str (metavar "FILE" <> help "IP range database")

writeFileWithRanges :: Params -> IO ()
writeFileWithRanges (Params n fp) = do
  s <- show <$> Gen.sample (Gen.resize 99 $ genIPRangeDBSized n n)
  writeFile fp s

main :: IO ()
main = (execParser opts >>= writeFileWithRanges)
       `catches` [Handler parserExit]
  where
    opts =
      info (mkParams <**> helper)
           (fullDesc <>
            progDesc ("Generates IP range database"))
    parserExit :: ExitCode -> IO ()
    parserExit _ = pure ()
