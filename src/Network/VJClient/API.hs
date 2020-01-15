{-# LANGUAGE OverloadedStrings #-}
-- | Raw API bindings of Virtual Judge
module Network.VJClient.API where
import Network.Hequests.Simple (post, post', get_, Response, SessionIO)
import Network.VJClient.Constants
import Network.VJClient.Types
import Network.VJClient.ToJSON
import Data.ByteString.Char8 (unpack)
import qualified Network.Hequests.Request as R

login :: String -- ^ Username
      -> String -- ^ Password
      -> SessionIO Response
login username password = post u q
  where u = urlOfAction VJLogin
        q = [("username", username), ("password", password)]

create :: SessionIO Response
create = get_ u
  where u = urlOfAction VJCreate

findPS :: String -- ^ OJ
       -> String -- ^ Problem ID
       -> SessionIO Response
findPS oj prob = post u q
  where u = urlOfAction VJFindProblemSimple
        q = [ ("oj", oj)
            , ("problemId", prob)]

edit :: VJContest -> SessionIO Response
edit = postJSON u
  where u = urlOfAction VJEdit

postJSON :: ToJSON p
         => String -- ^ URL
         -> p -- ^ The json object to be posted
         -> SessionIO Response
postJSON u json = post' config u []
  where jsonHeader = ("Content-Type", "application/json")
        defaultHeaders = R.headers R.defaultConfig
        config = R.defaultConfig { R.headers = (jsonHeader:) <$> defaultHeaders, R.body = Just $ encodeJSON json }
