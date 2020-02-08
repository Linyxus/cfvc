module App.CFVC.Types
  ( liftIO
  , liftLoad
  , liftVJClient
  , liftSessionIO
  , App(..)
  , AppError(..)
  ) where
import Network.VJClient.Types
import App.CFVC.Config.Types
import Control.Monad.Except
import Control.Monad.Identity (runIdentity)

data AppError = ClientError VJError
              | ConfigLoadError String
              deriving (Eq, Show)

type App = ExceptT AppError IO

liftLoad :: Load a -> App a
liftLoad = mapExceptT (return . f . runIdentity)
  where f (Left s) = Left $ ConfigLoadError s
        f (Right x) = return x

liftVJClient :: VJClient a -> App a
liftVJClient m = ExceptT $ f <$> runClient m
  where f (Left e) = Left $ ClientError e
        f (Right x) = return x

liftSessionIO = liftVJClient . liftSIO
