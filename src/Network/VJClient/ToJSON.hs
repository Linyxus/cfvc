{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.VJClient.ToJSON where
import Data.ByteString.Char8 (ByteString, pack, unpack, intercalate, putStrLn)
import Control.Monad.Writer (Writer, execWriter, tell)

class ToJSON a where
  encodeJSON :: a -> ByteString

instance ToJSON Bool where
  encodeJSON True = "1"
  encodeJSON False = "0"

instance ToJSON Word where
  encodeJSON = pack . show

instance ToJSON ByteString where
  encodeJSON x = "\"" <> x <> "\""

instance ToJSON Int where
  encodeJSON = pack . show

instance ToJSON a => ToJSON [a] where
  encodeJSON = ("["<>) . (<>"]") . intercalate "," . fmap encodeJSON

instance ToJSON (EncodeObject ()) where
  encodeJSON = encodeObject

type EncodeObject = Writer [ByteString]

data JNull = JNull deriving (Eq, Show)

instance ToJSON JNull where
  encodeJSON _ = "null"

infixr 1 .=
(.=) :: ToJSON a => ByteString -> a -> EncodeObject ()
key .= val = tell . pure $ "\"" <> key <> "\":" <> encodeJSON val

encodeObject :: EncodeObject () -> ByteString
encodeObject m = "{" <> intercalate "," (execWriter m) <> "}"

emptyObject :: EncodeObject ()
emptyObject = return ()
