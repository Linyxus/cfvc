module App.CFVC.Types
  ( liftIO
  , liftLoad
  , liftVJClient
  , liftSessionIO
  , liftEither
  , runApp
  , executeApp
  , App(..)
  , AppError(..)
  , module Network.VJClient.Types
  , module App.CFVC.Config.Types
  ) where
import Network.VJClient.Types
import App.CFVC.Config.Types
import Control.Monad.Except
import Control.Monad.Identity (runIdentity)
import Data.Yaml (ParseException)
import Text.Pretty.Simple (pPrint)

data AppError = ClientError VJError
              | ConfigLoadError String
              | YamlParseError ParseException
              deriving (Show)

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

runApp :: App a -> IO (Either AppError a)
runApp = runExceptT

executeApp :: App a -> IO ()
executeApp = (>>= f) . runApp
  where f (Left e) = putStr "[Error]\n" >> pPrint e
        f _ = return ()
