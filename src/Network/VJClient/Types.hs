{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
module Network.VJClient.Types
  ( module Network.VJClient.Types
  , throwError
  ) where
import Network.Hequests.Request (evalSession_)
import Network.Hequests.Types
import Control.Monad.Except (ExceptT, runExceptT, lift, throwError)
import Network.VJClient.ToJSON
import Data.ByteString.Char8 (ByteString, pack)

data VJAction = VJLogin
              | VJCreate
              | VJEdit
              | VJCheckLoginStatus
              | VJFindProblemSimple

data VJAuth = VJAuth { vjUsername :: String
                     , vjPassword :: String
                     } deriving (Eq, Show)

data VJError = LoginError String
             | ProblemNotFound
             | ResponseParseError
             | UnknownError String
  deriving (Eq, Show)

type VJClient = ExceptT VJError SessionIO

runClient :: VJClient a -> IO (Either VJError a)
runClient = evalSession_ . runExceptT

liftSIO :: SessionIO a -> VJClient a
liftSIO = lift

data VJProblem = VJProblem { vjOj :: String
                           , vjPid :: Int
                           , vjProbNum :: String
                           , vjProbName :: String
                           } deriving (Eq, Show)

data VJAccess = Public
              | Protected String
              | Private String
              deriving (Eq, Show)

data VJContest = VJContest { vjBeginTime :: Int
                           , vjDesc :: String
                           , vjDescFmt :: String
                           , vjLength :: Int
                           , vjAccess :: VJAccess
                           , vjShowPeers :: Bool
                           , vjTitle :: String
                           , vjProblems :: [VJProblem]
                           , vjAnnouncement :: String
                           } deriving (Eq, Show)

instance ToJSON VJProblem where
  encodeJSON VJProblem{..} = encodeObject $ do
    "alias" .= ("" :: ByteString)
    "descId" .= (0 :: Int)
    "oj" .= pack vjOj
    "pid" .= vjPid
    "probNum" .= pack vjProbNum
    "weight" .= (1 :: Int)

instance ToJSON VJContest where
  encodeJSON VJContest{..} = encodeObject $ do
    "announcement" .= pack vjAnnouncement
    "beginTime" .= vjBeginTime
    "contestId" .= (0 :: Int)
    "description" .= do
      "format" .= pack vjDescFmt
      "content" .= pack vjDesc
    "groups" .= emptyObject
    "length" .= vjLength
    encodeAccess vjAccess
    "partialScore" .= False
    "penalty" .= (1200 :: Int)
    "problems" .= vjProblems
    "rankCellMeaning" .= JNull
    "showPeers" .= vjShowPeers
    "sumTime" .= True
    "timeMachine" .= True
    "type" .= (0 :: Int)
    "title" .= pack vjTitle

encodeAccess :: VJAccess -> EncodeObject ()
encodeAccess Public = "openness" .= (0 :: Int)
encodeAccess (Protected p) = do
  "openness" .= (1 :: Int)
  "password" .= pack p
encodeAccess (Private p) = do
  "openness" .= (2 :: Int)
  "password" .= pack p
