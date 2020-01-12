{-# LANGUAGE OverloadedStrings #-}
-- |

module Network.Hequests.Request
  ( module Network.Hequests.Request
  , module Network.HTTP.Conduit
  , module Network.HTTP.Simple
  ) where

import Network.Hequests.Types
import Data.ByteString (ByteString, concat)
import Network.HTTP.Simple
import Network.HTTP.Conduit hiding (httpLbs)
import qualified Control.Monad.State as ST

requestWithMethod :: ByteString -> RequestConfig -> String -> IO (Response ByteString)
requestWithMethod method config = httpBS
                                  . setRequestMethod method
                                  . buildRequest config
                                  . parseRequest_

get :: RequestConfig -> String -> IO (Response ByteString)
get = requestWithMethod "GET"

get_ = requestWithMethod "GET" defaultConfig

post = requestWithMethod "POST"

sessRequest :: ByteString -> RequestConfig -> String -> SessionIO (Response ByteString)
sessRequest method config url = do
  let req = setRequestMethod method . buildRequest config . parseRequest_ $ url
  cj <- ST.get
  let (name, value) = buildCookieHeader cj
  let req' = setRequestHeader name [value] req
  resp <- httpBS req'
  ST.put $ updateCookieMap cj $  responseCookieJar resp
  return resp

showCookie :: Cookie -> ByteString
showCookie ck = let name = cookie_name ck
                    value = cookie_value ck
                in Data.ByteString.concat [name, "=", value, "; "]

buildCookieHeader :: CookieMap -> (HeaderName, ByteString)
buildCookieHeader cj = ("Cookie", Data.ByteString.concat . map showCookie . destroyCookieMap $ cj)

sessGet = sessRequest "GET"

sessPost = sessRequest "POST"

runSession :: SessionIO a -> CookieMap -> IO (a, CookieMap)
runSession = ST.runStateT

runSession_ :: SessionIO a -> IO (a, CookieMap)
runSession_ = flip runSession emptyCookieMap

evalSession_ :: SessionIO a -> IO a
evalSession_ sess = ST.evalStateT sess emptyCookieMap

buildRequest :: RequestConfig -> Request -> Request
buildRequest config = buildQuery config . buildParams config . buildHeaders config
  where buildQuery (RequestConfig query _ _ _) req =
          case query of
            Just q -> setRequestQueryString q req
            Nothing -> req
        buildParams (RequestConfig _ params _ _) req =
          case params of
            Just p -> urlEncodedBody p req
            Nothing -> req
        buildHeaders (RequestConfig _ _ _ mheaders) =
          case mheaders of
            Just headers -> setRequestHeaders headers
            Nothing -> id
