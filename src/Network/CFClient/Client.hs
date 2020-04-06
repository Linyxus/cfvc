module Network.CFClient.Client where
import Network.CFClient.Types
import Network.CFClient.Constants
import Network.CFClient.Parsers
import Data.ByteString (ByteString)
import Network.Hequests.Simple
import qualified Network.Hequests.Request as R
import Control.Monad.Except

loadContestPage :: Int -> CFClient ByteString
loadContestPage x = lift (get1_ $ urlOf x) >>= g
  where g :: Response -> CFClient ByteString
        g resp
          | R.getResponseStatusCode resp == 200 = return $ R.responseBody resp
          | otherwise = throwError NetworkError

loadContestProbNums :: Int -> CFClient [String]
loadContestProbNums n = loadContestPage n >>= parseProblems n
