{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Parsers for parsing response.
-- Since responses from Virtual Judge only covers a small subset
-- of JSON, parsers are written in attoparsec instead of aeson
module Network.VJClient.Parsers
  ( parseVJProblem
  ) where
import Network.VJClient.Types
import Prelude hiding (takeWhile)
import Data.Attoparsec.ByteString
import Data.Word8 (Word8, _quotedbl, _comma, _bracketleft, isSpace, isDigit)
import Data.ByteString (ByteString)
import Control.Applicative
import Control.Monad (when)
import Data.ByteString.Char8 (unpack)
import Control.Monad.Except (runExceptT, throwError, ExceptT, lift)

doubleQuoteP :: Parser Word8
doubleQuoteP = word8 _quotedbl

bracketLeftP :: Parser Word8
bracketLeftP = word8 _bracketleft

commaP :: Parser Word8
commaP = word8 _comma

nullP :: Parser ByteString
nullP = string "null"

stringP :: Parser ByteString
stringP = doubleQuoteP >> takeTill (== _quotedbl) <* doubleQuoteP

bs2int :: ByteString -> Int
bs2int = read . unpack

intP :: Parser Int
intP = bs2int <$> takeWhile1 isDigit

sc :: Parser ()
-- ^ Space consumer
sc = () <$ takeWhile isSpace

-- | Response of findProblemSimple API
-- Format:
-- Not found: [null,null,null,"Codeforces","12"]
-- Not found: [3042780,"N/A",1,"CodeForces","1"]
-- Good: [2987875,"Three Friends",0,"CodeForces","1272a"]
data FindPSItem = FPSNull
                | FPSString ByteString
                | FPSInt Int
                deriving (Eq, Show)

findPSItemP :: Parser FindPSItem
findPSItemP = choice
  [ FPSNull <$ nullP
  , FPSString <$> stringP
  , FPSInt <$> intP
  ]

type ParserMaybe = ExceptT () Parser

pVJProblem :: Parser (Maybe VJProblem)
pVJProblem = fmap f . runExceptT $ p
  where p :: ParserMaybe VJProblem
        p = do
          lift $ sc >> bracketLeftP >> sc
          pid <- lift q
          vjPid <- getPid pid
          pname <- lift q
          vjProbName <- getStr pname
          ind <- lift q
          indicator ind
          oj <- lift q
          vjOj <- getStr oj
          pn <- lift findPSItemP
          vjProbNum <- getStr pn
          return VJProblem{..}
        f (Left _) = Nothing
        f (Right x) = Just x
        q = findPSItemP <* (sc >> commaP >> sc)
        getPid :: FindPSItem -> ParserMaybe Int
        getPid (FPSInt i) = return i
        getPid _ = throwError ()
        getStr :: FindPSItem -> ParserMaybe String
        getStr (FPSString s) = return $ unpack s
        getStr _ = throwError ()
        indicator :: FindPSItem -> ParserMaybe ()
        indicator (FPSInt 0) = return ()
        indicator _ = throwError ()

parseVJProblem :: ByteString -> VJClient VJProblem
parseVJProblem s = case parseOnly pVJProblem s of
                     Left _ -> throwError ResponseParseError
                     Right Nothing -> throwError ProblemNotFound
                     Right (Just p) -> return p
