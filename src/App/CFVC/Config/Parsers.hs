module App.CFVC.Config.Parsers
  ( parseTime
  ) where

import App.CFVC.Config.Types
import Data.Attoparsec.ByteString
import Data.ByteString.Char8 (ByteString, pack, unpack)
import Data.Word8
import Control.Applicative

bs2int :: ByteString -> Int
bs2int = read . unpack

intP :: Parser Int
intP = bs2int <$> takeWhile1 isDigit

colonP :: Parser Word8
colonP = word8 _colon

timeP :: Parser Time
timeP = Time <$> offsetP <*> timeDeltaP

timeDeltaP :: Parser TimeDelta
timeDeltaP = TimeDelta <$> intP <*> p <*> p
  where p = colonP >> intP

offsetP :: Parser Bool
offsetP = f <$> optional plusP
  where f Nothing = False
        f _ = True

plusP :: Parser Word8
plusP = word8 _plus

parseTime :: String -> Either String Time
parseTime = parseOnly timeP . pack
