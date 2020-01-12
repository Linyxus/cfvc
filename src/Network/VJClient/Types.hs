{-# LANGUAGE KindSignatures #-}
module Network.VJClient.Types
  ( module Network.VJClient.Types
  , throwError
  ) where
import Network.Hequests.Request (evalSession_)
import Network.Hequests.Types
import Control.Monad.Except (ExceptT, runExceptT, lift, throwError)

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
