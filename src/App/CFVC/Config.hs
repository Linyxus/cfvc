{-# LANGUAGE RecordWildCards #-}
module App.CFVC.Config where
import App.CFVC.Types
import App.CFVC.Config.Types
import App.CFVC.Config.Parsers
import App.CFVC.Config.Time
import Network.VJClient.Types
import Network.VJClient.Client
import Control.Monad.Except

getField :: String -> Maybe a -> Load a
getField field Nothing = throwError $ "Field " <> field <> " is required"
getField _ (Just x) = return x

getTime :: String -> Maybe String -> Load Time
getTime name mf = withExcept f $ getField name mf >>= liftEither . parseTime
  where f = const $ "Parse error while parsing time " <> name

getTimeDelta :: String -> Maybe String -> Except String TimeDelta
getTimeDelta n m = f <$> getTime n m
  where f (Time _ d) = d

getAccess :: LoadConfig -> Load VJAccess
getAccess LoadConfig{..} = getField "access-level" lcAccessLevel >>= f
  where f 0 = return Public
        f l = g l <$> getField "password" lcPassword
        g 1 = Protected
        g 2 = Private

loadConfig :: LoadConfig -> Load AppConfig
loadConfig lc@LoadConfig{..} = do
  acTitle <- getField "title" lcTitle
  acDesc <- getField "description" lcDesc
  acAnnouncement <- getField "announcement" lcAnnouncement
  acShowPeers <- getField "show-peers" lcShowPeers
  acBeginTime <- getTime "begin-time" lcBeginTime
  acAuthUser <- getField "auth-username" lcAuthUser
  acAuthPassword <- getField "auth-password" lcAuthPassword
  acLength <- getTimeDelta "length" lcLength
  acAccess <- getAccess lc
  let acProblems = lcProblems
  return AppConfig{..}

resolve :: AppConfig -> App VJContest
resolve AppConfig{..} = do
  let vjTitle = acTitle
      vjDesc = acDesc
      vjDescFmt = "MD"
      vjShowPeers = acShowPeers
      vjAnnouncement = acAnnouncement
      vjLength = timeDeltaToMS acLength
      vjAccess = acAccess
      cred = VJAuth acAuthUser acAuthPassword
  vjBeginTime <- liftIO $ resolveTime acBeginTime
  vjProblems <- liftVJClient $ resolveProblems cred acProblems
  return VJContest{..}

resolveProblems :: VJAuth -> [String] -> VJClient [VJProblem]
resolveProblems cred probs = vjLogin cred >> mapM (vjFindProblem "codeforces") probs
