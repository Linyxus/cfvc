-- | Virtual Judge Client
{-# LANGUAGE OverloadedStrings #-}
module Network.VJClient.Client where
import Network.VJClient.Types
import Network.VJClient.Constants
import Network.VJClient.Parsers
import Network.Hequests.Simple (post, Response)
import Data.ByteString.Char8 (unpack)
import qualified Network.VJClient.API as API
import qualified Network.Hequests.Request as R

vjLogin :: VJAuth -> VJClient ()
vjLogin (VJAuth u p) = liftSIO (API.login u p) >>= f
  where url = urlOfAction VJLogin
        q = [("username", u), ("password", p)]
        f :: Response -> VJClient ()
        f resp
          | R.getResponseStatusCode resp == 200 =
              case R.responseBody resp of
                "success" -> return ()
                e -> throwError . LoginError $ unpack e
          | otherwise = throwError $ LoginError "Invalid Status Code"

vjFindProblem :: String -- ^ OJ
              -> String -- ^ Problem ID
              -> VJClient VJProblem
vjFindProblem oj prob = liftSIO (API.findPS oj prob) >>= f
  where f :: Response -> VJClient VJProblem
        f resp
          | R.getResponseStatusCode resp == 200 = parseVJProblem $ R.responseBody resp
          | otherwise = throwError $ UnknownError $ "Bad status code while fetching information for problem " <> prob

vjCreateContest :: VJContest
                -> VJClient Int -- ^ Contest ID
vjCreateContest c = liftSIO API.create >> liftSIO (API.edit c) >>= f
  where f :: Response -> VJClient Int
        f resp
          | R.getResponseStatusCode resp == 200 = parseVJContestId $ R.responseBody resp
          | otherwise = throwError $ UnknownError "Bad Status code while creating contests"
