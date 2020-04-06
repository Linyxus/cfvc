module Network.CFClient.Types
  ( CFClient(..)
  , CFError(..)
  , runClient
  ) where
import Control.Monad.Except

type CFClient = ExceptT CFError IO

data CFError = NetworkError
             | ResponseParseError
             deriving Show

runClient :: CFClient a -> IO (Either CFError a)
runClient = runExceptT
