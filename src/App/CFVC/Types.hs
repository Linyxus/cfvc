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
import qualified Network.VJClient.Types as VJ
import Network.CFClient.Types
import qualified Network.CFClient.Types as CF
import App.CFVC.Config.Types
import Control.Monad.Except
import Control.Monad.Identity (runIdentity)
import Data.Yaml (ParseException)
import Text.Pretty.Simple (pPrint)

data AppError = VJClientError VJError
              | CFClientError CFError
              | ConfigLoadError String
              | YamlParseError ParseException
              deriving (Show)

type App = ExceptT AppError IO

liftLoad :: Load a -> App a
liftLoad = mapExceptT (return . f . runIdentity)
  where f (Left s) = Left $ ConfigLoadError s
        f (Right x) = return x

liftVJClient :: VJClient a -> App a
liftVJClient m = ExceptT $ f <$> VJ.runClient m
  where f (Left e) = Left $ VJClientError e
        f (Right x) = return x

liftCFClient :: CFClient a -> App a
liftCFClient m = ExceptT $ f <$> CF.runClient m
  where f (Left e) = Left $ CFClientError e
        f (Right x) = return x

liftSessionIO = liftVJClient . liftSIO

runApp :: App a -> IO (Either AppError a)
runApp = runExceptT

executeApp :: App a -> IO ()
executeApp = (>>= f) . runApp
  where f (Left e) = putStr "[Error]\n" >> pPrint e
        f _ = return ()
