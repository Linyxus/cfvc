module Network.Hequests.Simple
  ( get, get', get_, get1', get1, get1_
  , post', post, post1', post1
  , URL, Query, Response
  , module Network.Hequests.Types
  ) where
import qualified Network.Hequests.Request as Req
import Network.Hequests.Types
import Data.ByteString.Char8 (pack, ByteString)

type URL = String

type Query = [(String, String)]

type Response = Req.Response ByteString
-- ^ Standard response of Hequests

get' :: RequestConfig -> URL -> Query -> SessionIO Response
-- ^ Get action with configuration specified. `q` will override
-- the query field of the config
get' conf u q = Req.sessGet c' u
  where c' = conf { query = Just $ packQuery q }

get :: URL -> Query -> SessionIO Response
-- ^ Get action with default configuration
get = get' defaultConfig

get_ :: URL -> SessionIO (Req.Response ByteString)
-- ^ Get action without query items
get_ = flip get []

get1' :: RequestConfig -> URL -> Query -> IO Response
-- ^ Directly perform a get action, not storing the session (that is,
-- a one-time get request)
get1' conf u q = Req.evalSession_ $ get' conf u q

get1 :: URL -> Query -> IO Response
-- ^ Similar to get, but directly perform the action
get1 = get1' defaultConfig

get1_ :: URL -> IO Response
-- ^ Similar to get_, but directly perform the action
get1_ = flip get1 []

post' :: RequestConfig -> URL -> Query -> SessionIO Response
-- ^ Post action with config specified. `q` will override the params field
-- of the config
post' c u q = Req.sessPost c' u
  where c' = c { params = Just $ packForm q }

post :: URL -> Query -> SessionIO Response
-- ^ Post action with default config
post = post' defaultConfig

post1' :: RequestConfig -> URL -> Query -> IO Response
post1' c u q = Req.evalSession_ $ post' c u q

post1 :: URL -> Query -> IO Response
post1 = post1' defaultConfig

packQuery :: Query -> [(ByteString, Maybe ByteString)]
-- ^ Pack query into the format required for Hequests
packQuery = fmap f
  where f (k, v) = (pack k, Just . pack $ v)

packForm :: Query -> [(ByteString, ByteString)]
-- ^ Pack query into POST form data
packForm = fmap f
  where f (k, v) = (pack k, pack v)
