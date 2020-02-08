{-# LANGUAGE RecordWildCards #-}
module App.CFVC.Config.Time
  ( resolveTime
  , timeDeltaToMS
  ) where

import App.CFVC.Config.Types
import Data.Time.Clock.POSIX
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time
import Data.Int

timeDeltaToMS :: TimeDelta -> Int
timeDeltaToMS TimeDelta{..} = 1000 * (hour * 3600 + minute * 60 + second)

resolveDelta :: TimeDelta -> IO Int
resolveDelta d = (+t) <$> posixNow
  where t = timeDeltaToMS d

nowTimeToMS :: IO Int
nowTimeToMS = do
  t <- getCurrentTime
  tz <- getCurrentTimeZone
  return $ timeOfDayToMS . localTimeOfDay . utcToLocalTime tz $ t

dayInMS :: Int
dayInMS = 24 * 60 * 60 * 1000

resolveAbs :: TimeDelta -> IO Int
resolveAbs d = do
  nowTime <- nowTimeToMS
  now <- posixNow
  return $ now + scheduled - nowTime + if scheduled > nowTime then 0 else dayInMS
  where scheduled = timeDeltaToMS d

posixNow :: IO Int
posixNow = (*1000) . round <$> getPOSIXTime

resolveTime :: Time -> IO Int
resolveTime (Time True t) = resolveDelta t
resolveTime (Time False t) = resolveAbs t

timeOfDayToMS :: TimeOfDay -> Int
timeOfDayToMS t = 1000 * (h * 3600 + m * 60 + s)
  where h = todHour t
        m = todMin t
        s = round $ todSec t
