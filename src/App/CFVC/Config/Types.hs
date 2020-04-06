module App.CFVC.Config.Types where
import Network.VJClient.Types (VJAccess)
import Control.Monad.Except

type Load = Except String

data LoadConfig = LoadConfig { lcTitle :: Maybe String
                             , lcContest :: Maybe Int
                             , lcProblems :: [String]
                             , lcBeginTime :: Maybe String
                             , lcLength :: Maybe String
                             , lcDesc :: Maybe String
                             , lcAnnouncement :: Maybe String
                             , lcAccessLevel :: Maybe Int
                             , lcPassword :: Maybe String
                             , lcShowPeers :: Maybe Bool
                             , lcAuthUser :: Maybe String
                             , lcAuthPassword :: Maybe String
                             } deriving (Eq, Show)

data AppConfig = AppConfig { acTitle :: String
                           , acContest :: Maybe Int
                           , acProblems :: [String]
                           , acBeginTime :: Time
                           , acLength :: TimeDelta
                           , acDesc :: String
                           , acAnnouncement :: String
                           , acAccess :: VJAccess
                           , acShowPeers :: Bool
                           , acAuthUser :: String
                           , acAuthPassword :: String
                           } deriving (Eq, Show)

data CLIConfig = CLIConfig { clContestId :: Maybe Int
                           , clTitle :: Maybe String
                           , clBeginTime :: Maybe String
                           , clLength :: Maybe String
                           , clDesc :: Maybe String
                           , clAnnouncement :: Maybe String
                           , clAuthUser :: Maybe String
                           , clAuthPassword :: Maybe String
                           } deriving (Eq, Show)

data Time = Time { offset :: Bool
                 , timeDelta :: TimeDelta
                 } deriving (Eq, Show)

data TimeDelta = TimeDelta { hour :: Int
                           , minute :: Int
                           , second :: Int
                           } deriving (Eq, Show)
