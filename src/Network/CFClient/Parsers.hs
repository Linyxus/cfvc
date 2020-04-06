{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.CFClient.Parsers where
import Data.Attoparsec.ByteString
import Data.ByteString.Char8 (ByteString, unpack, pack)
import qualified Data.ByteString as BS
import Data.Word8
import Data.String
import Control.Applicative
import Control.Monad.Except (throwError)

import Network.CFClient.Types

instance (a ~ ByteString) => IsString (Parser a) where
  fromString = string . pack

ltP :: Parser Word8
ltP = word8 _less

gtP :: Parser Word8
gtP = word8 _greater

slashP :: Parser Word8
slashP = word8 _slash

equalP :: Parser Word8
equalP = word8 _equal

tag :: Parser a -> Parser a
tag p = ltP >> p <* gtP

closingTag :: Parser a -> Parser a
closingTag p = ltP >> slashP >> p <* gtP

upperP :: Parser Word8
upperP = satisfy isUpper

digitP :: Parser Word8
digitP = satisfy isDigit

spaceP :: Parser Word8
spaceP = satisfy isSpace

space :: Parser [Word8]
space = many spaceP

pProbNum :: Parser String
pProbNum = fmap (unpack . BS.pack) $ liftA2 (:) upperP $ many digitP

pProbUrl :: Int -> Parser String
pProbUrl n = string ("/contest/" <> f n <> "/problem/") >> pProbNum
  where f = pack . show

pProbTag :: Int -> Parser String
pProbTag n = tag (string "a href=\"" >> pProbUrl n <* string "\"") >>= close
  where close prob = space >> string (pack prob) >> space >> closingTag (string "a") >> return prob

pNextProb :: Int -> Parser String
pNextProb n = consumer >> pProbTag n <|> (ltP >> consumer >> pNextProb n)

pProblems :: Int -> Parser [String]
pProblems n = many $ pNextProb n

parseProblems :: Int -> ByteString -> CFClient [String]
parseProblems n src = f $ g <$> parseOnly (pProblems n) src
  where f (Left e) = throwError ResponseParseError
        f (Right []) = throwError ResponseParseError
        f (Right x) = return x
        g = fmap (show n <>)

consumer :: Parser ByteString
consumer = takeTill (== _less)
