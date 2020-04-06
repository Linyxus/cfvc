module Network.CFClient.Constants where

contestRootPath :: String
contestRootPath = "https://codeforces.com/contest/"

urlOf :: Int -> String
urlOf = (contestRootPath <>) . show
