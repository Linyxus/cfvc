module Network.VJClient.Constants where
import Network.VJClient.Types
import qualified Data.ByteString as BS

rootURL :: String
rootURL = "https://vjudge.net"

loginPath :: String
loginPath = "/user/login"

createPath :: String
createPath = "/contest/create"

editPath :: String
editPath = "/contest/edit"

findPSPath :: String
-- ^ Find Problem Simple
findPSPath = "/problem/findProblemSimple"

urlOfAction :: VJAction -> String
urlOfAction act = rootURL <> f act
  where f VJLogin = loginPath
        f VJCreate = createPath
        f VJEdit = editPath
        f VJFindProblemSimple = findPSPath
