module App.CFVC where
import App.CFVC.Types
import App.CFVC.Config
import Network.VJClient.Client
import System.Directory (getAppUserDataDirectory)
import Text.Pretty.Simple (pPrint)

appName = "cfvc"
configFile = "config.yaml"

getConfigPath :: IO String
getConfigPath = (<> configFile) . (<> "/") <$> getAppUserDataDirectory appName

createContest :: VJAuth -> VJContest -> App Int
createContest auth contest = liftVJClient $ vjLogin auth >> vjCreateContest contest

urlOf :: Int -> String
urlOf = ("https://vjudge.net/contest/" <>) . show

main :: IO ()
main = executeApp $ do
  liftIO $ putStrLn "Hey! Here is cfvc."
  liftIO $ putStr "[Info] Loading config from " >> getConfigPath >>= putStrLn
  (auth, contest) <- liftIO getConfigPath >>= loadApp
  liftIO $ putStrLn "[Info] Config loaded.\n[Info] Auth information:"
  pPrint auth
  liftIO $ putStrLn "[Info] The contest to be created:"
  pPrint contest
  liftIO $ putStrLn "[Info] Creating contest ..."
  cid <- createContest auth contest
  liftIO $ putStrLn $ "[Info] Done! Contest URL: " <> urlOf cid
