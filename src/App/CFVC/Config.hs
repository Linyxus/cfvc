{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module App.CFVC.Config
  ( loadApp
  , module App.CFVC.Types
  ) where
import App.CFVC.Types
import App.CFVC.Config.Types
import App.CFVC.Config.Parsers
import App.CFVC.Config.Time
import App.CFVC.Config.Yaml
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

getAuth :: AppConfig -> VJAuth
getAuth AppConfig{..} = let vjUsername = acAuthUser
                            vjPassword = acAuthPassword
                        in VJAuth{..}

resolveProblems :: VJAuth -> [String] -> VJClient [VJProblem]
resolveProblems cred probs = vjLogin cred >> mapM (vjFindProblem "codeforces") probs

parseYaml :: String -> App LoadConfig
parseYaml = ExceptT . fmap f . loadYaml
  where f (Left e) = Left $ YamlParseError e
        f (Right x) = return x

loadApp :: String -> App (VJAuth, VJContest)
loadApp path = parseYaml path >>= liftLoad . loadConfig >>= f
  where f ac = (getAuth ac,) <$> resolve ac
