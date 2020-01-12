{-# LANGUAGE OverloadedStrings #-}
module Network.Hequests.Types where

import Network.HTTP.Simple
import Network.HTTP.Conduit
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI)
import Control.Monad.State
import qualified Data.Map as M

data AuthInfo = BasicAuth ByteString ByteString

type HeaderName = CI ByteString

data RequestConfig = RequestConfig { query :: Maybe Query
                                   , params :: Maybe [(ByteString, ByteString)]
                                   , auth :: Maybe AuthInfo
                                   , headers :: Maybe [(HeaderName, ByteString)]}

pythonHeaders = [ ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/76.0.3809.132 Safari/537.36")
                , ("Accept-Encoding", "gzip, deflate")
                , ("Accept", "*/*")
                , ("Connection", "keep-alive")
                ]

defaultConfig = RequestConfig Nothing Nothing Nothing (Just pythonHeaders)

newtype CookieMap = CookieMap { getCookieMap :: M.Map ByteString Cookie }

updateCookieMap :: CookieMap -> CookieJar -> CookieMap
updateCookieMap cm cj = updateCookieMap' cm $ destroyCookieJar cj

updateCookieMap' :: CookieMap -> [Cookie] -> CookieMap
updateCookieMap' cm [] = cm
updateCookieMap' (CookieMap m) (cookie:cs) = updateCookieMap' (CookieMap $ M.insert name cookie' m) cs
  where compareResult :: Cookie -> Maybe Cookie -> Cookie
        compareResult c Nothing = c
        compareResult c1 (Just c2)
          | t1 > t2 = c1
          | otherwise = c2
          where t1 = cookie_creation_time c1
                t2 = cookie_creation_time c2
        name = cookie_name cookie
        cookie' = compareResult cookie (M.lookup name m)

destroyCookieMap :: CookieMap -> [Cookie]
destroyCookieMap = fmap (\(_, x) -> x) . M.toList . getCookieMap

emptyCookieMap = CookieMap $ M.fromList []

type SessionIO = StateT CookieMap IO
