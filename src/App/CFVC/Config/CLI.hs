module App.CFVC.Config.CLI where
import Options.Applicative
import App.CFVC.Config.Types

pArgs :: Parser CLIConfig
pArgs = CLIConfig <$> pContest <*> pTitle
                  <*> pBeginTime <*> pLength
                  <*> pDesc <*> pAnn
                  <*> pAuthUser <*> pAuthPassword
  where pContest = optional $ argument auto (metavar "CONTEST_ID")
        pTitle = optional $ strOption $ long "title"
                                     <> metavar "TITLE"
                                     <> short 't'
                                     <> help "Title of VJudge contest"
        pBeginTime = optional $ strOption $ long "begin-time"
                                         <> metavar "TIME"
                                         <> short 'b'
                                         <> help "Begin time: 18:00:00, +1:30:00 ..."
        pLength = optional $ strOption $ long "length"
                                      <> metavar "TIME"
                                      <> short 'l'
                                      <> help "Contest length: 3:00:00"
        pDesc = optional $ strOption $ long "description"
                                    <> metavar "TEXT"
                                    <> short 'd'
                                    <> help "Contest description"
        pAnn = optional $ strOption $ long "announcement"
                                   <> metavar "TEXT"
                                   <> short 'a'
                                   <> help "Contest announcement"
        pAuthUser = optional $ strOption $ long "username"
                                        <> metavar "USERNAME"
                                        <> short 'u'
                                        <> help "Virtual Judge username"
        pAuthPassword = optional $ strOption $ long "password"
                                            <> metavar "PASSWORD"
                                            <> short 'p'
                                            <> help "Virtual Judge password"

parseArgs :: IO CLIConfig
parseArgs = execParser p
  where p = info (pArgs <**> helper) $ fullDesc
                                    <> progDesc "Clone CodeForces contest onto Virtual Judge"
                                    <> header "cfvc - CodeForces Virtual Cloner"
