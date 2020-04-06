{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module App.CFVC.Config
  ( loadApp
  , module App.CFVC.Types
  , yamlTemplate
  , parseCLI
  ) where
import App.CFVC.Types
import App.CFVC.Config.Types
import App.CFVC.Config.Parsers
import App.CFVC.Config.Time
import App.CFVC.Config.Yaml
import App.CFVC.Config.CLI
import Network.VJClient.Types
import Network.VJClient.Client
import Network.CFClient.Types
import Network.CFClient.Client
import Control.Monad.Except
import Control.Applicative ((<|>))

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

loadConfig :: CLIConfig -> LoadConfig -> Load AppConfig
loadConfig cl@CLIConfig {..} lc@LoadConfig{..} = do
  acTitle <- getField "title" $ clTitle <|> lcTitle
  acDesc <- getField "description" $ clDesc <|> lcDesc
  acAnnouncement <- getField "announcement" $ clAnnouncement <|> lcAnnouncement
  acShowPeers <- getField "show-peers" lcShowPeers
  acBeginTime <- getTime "begin-time" $ clBeginTime <|> lcBeginTime
  acAuthUser <- getField "username" $ clAuthUser <|> lcAuthUser
  acAuthPassword <- getField "password" $ clAuthPassword <|> lcAuthPassword
  acLength <- getTimeDelta "length" $ clLength <|> lcLength
  acAccess <- getAccess lc
  let acProblems = lcProblems
      acContest = clContestId <|> lcContest
  return AppConfig{..}

resolveContest :: Int -> App [String]
resolveContest = liftCFClient . loadContestProbNums

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
  contestProbs <- case acContest of
                   Nothing -> return []
                   Just x -> resolveContest x
  vjProblems <- liftVJClient $ resolveProblems cred $ acProblems <> contestProbs
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

parseCLI :: App CLIConfig
parseCLI = liftIO parseArgs

loadApp :: CLIConfig -> String -> App (VJAuth, VJContest)
loadApp cli path = parseYaml path >>= liftLoad . loadConfig cli >>= f
  where f ac = (getAuth ac,) <$> resolve ac
