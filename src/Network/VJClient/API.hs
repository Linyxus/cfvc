-- | Raw API bindings of Virtual Judge
module Network.VJClient.API where
import Network.Hequests.Simple (post, get_, Response, SessionIO)
import Network.VJClient.Constants
import Network.VJClient.Types
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
