{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module App.CFVC.Config.Yaml where
import App.CFVC.Config.Types
import Text.RawString.QQ
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
    <*> v .:? "contest"
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

yamlTemplate :: String
yamlTemplate
  = [r|
# Title of the contest on Vjudge
title: "A Good Contest on CodeForces"
# Id of the CodeForces contest to be cloned, optional
contest: 1328
# Additional problems, optional
problems:
  - "1212a"
# Begin time of the contest. It can either be absolute or relative.
# Absolute: 13:30:00 means the nearest 13:00:00 in the future
# Relative: +1:00:00 means 1 hour later
begin-time: 13:30:00
# Length of the contest
length: 24:0:0
# Description of the contest
desc: ""
# Annoucement of the contest
announcement: ""
# 0: Public
# 1: Protected
# 2: Private
access-level: 2
# Password of the contest if access level is either protected or private
password: "password"
# Show peers
show-peers: true
# VJudge username
auth-username:
# VJudge password
auth-password: |]
