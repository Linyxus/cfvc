{-# LANGUAGE RecordWildCards #-}
module App.CFVC where
import App.CFVC.Types
import App.CFVC.Config
import Network.VJClient.Client
import System.Directory (getAppUserDataDirectory, doesPathExist)
import Text.Pretty.Simple (pPrint)

appName = "cfvc"
configFile = "config.yaml"

getConfigPath :: IO String
getConfigPath = (<> configFile) . (<> "/") <$> getAppUserDataDirectory appName

configExists :: IO Bool
configExists = getConfigPath >>= doesPathExist

writeYamlTemp :: String -> IO ()
writeYamlTemp = flip writeFile yamlTemplate

maybeInitConfig :: IO ()
maybeInitConfig = configExists >>= go
  where go False = do
          putStrLn "[Info] No config file detected."
          p <- getConfigPath
          writeYamlTemp p
          putStrLn $ "[Info] Template config has been written to " <> p
          putStrLn "[Info] You may have to add necessary information into the config file"
        go True = return ()

createContest :: VJAuth -> VJContest -> App Int
createContest auth contest = liftVJClient $ vjLogin auth >> vjCreateContest contest

urlOf :: Int -> String
urlOf = ("https://vjudge.net/contest/" <>) . show

main :: IO ()
main = executeApp $ do
  cliConfig@CLIConfig{..} <- parseCLI
  liftIO $ putStrLn "Hey! Here is cfvc."
  liftIO maybeInitConfig
  liftIO $ putStr "[Info] Loading config from " >> getConfigPath >>= putStrLn
  (auth, contest) <- liftIO getConfigPath >>= loadApp cliConfig
  liftIO $ putStrLn "[Info] Config loaded.\n[Info] Auth information:"
  pPrint auth
  liftIO $ putStrLn "[Info] The contest to be created:"
  pPrint contest
  liftIO $ putStrLn "[Info] Creating contest ..."
  cid <- createContest auth contest
  liftIO $ putStrLn $ "[Info] Done! Contest URL: " <> urlOf cid
