{-# LANGUAGE OverloadedStrings #-}
module App.CFVC.Config.Yaml where
import App.CFVC.Config.Types
import App.CFVC.Types
import Data.Yaml ( ParseException
                 , decodeFileEither
                 , FromJSON(..)
                 , withObject
                 , (.:), (.:?), (.!=)
                 )

instance FromJSON LoadConfig where
  parseJSON = withObject "config" $ \v -> LoadConfig
    <$> v .:? "title"
    <*> v .:? "problems" .!= []
    <*> v .:? "begin-time"
    <*> v .:? "length"
    <*> v .:? "desc"
    <*> v .:? "announcement"
    <*> v .:? "access-level"
    <*> v .:? "password"
    <*> v .:? "show-peers"
    <*> v .:? "auth-username"
    <*> v .:? "auth-password"

loadYaml :: String -> IO (Either ParseException LoadConfig)
loadYaml = decodeFileEither
