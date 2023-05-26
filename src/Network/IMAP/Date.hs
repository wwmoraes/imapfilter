{-# OPTIONS_GHC -Wno-missing-fields #-}
module Network.IMAP.Date where

import Data.Fixed   (HasResolution (resolution), divMod')
import Data.Functor ((<&>))
import Data.Time    (FormatTime, LocalTime (localDay, localTimeOfDay),
                     TimeOfDay (todHour, todMin, todSec),
                     TimeZone (timeZoneMinutes, timeZoneName, timeZoneSummerOnly),
                     ZonedTime (zonedTimeToLocalTime, zonedTimeZone),
                     defaultTimeLocale, formatTime, getZonedTime, isLeapYear)

import Data.Time.Calendar.MonthDay    (dayOfYearToMonthAndDay)
import Data.Time.Calendar.OrdinalDate (toOrdinalDate)


import           Control.Arrow (Arrow (second))
import qualified Data.Time     as DT
import qualified System.Locale as SL
import qualified System.Time   as ST

now :: IO String
now = rfc2822DateFormat <$> getZonedTime

-- https://datatracker.ietf.org/doc/html/rfc2822#section-3.3
rfc2822DateFormat :: FormatTime t => t -> String
rfc2822DateFormat = formatTime defaultTimeLocale "%a, %-e %b %Y %T %z"

oldNow :: IO String
oldNow = (ST.getClockTime >>= ST.toCalendarTime) <&> oldRfc2822DateFormat

oldRfc2822DateFormat :: ST.CalendarTime -> String
oldRfc2822DateFormat = ST.formatCalendarTime SL.defaultTimeLocale "%a, %-e %b %Y %T %Z"

toOldCalendarTime :: ZonedTime -> ST.CalendarTime
toOldCalendarTime zt
  = ST.CalendarTime
      { ST.ctYear = fromIntegral year
      , ST.ctMonth = toEnum (moy-1)
      , ST.ctDay = dom
      , ST.ctHour = todHour t
      , ST.ctMin = todMin t
      , ST.ctSec = sec
      , ST.ctPicosec = picosec
      , ST.ctWDay = toEnum $ fromEnum dow
      , ST.ctYDay = doy
      , ST.ctTZName = timeZoneName tz
      , ST.ctTZ = 60 * timeZoneMinutes tz
      , ST.ctIsDST = timeZoneSummerOnly tz
      }
  where
    (d, t) = (,) <$> localDay <*> localTimeOfDay $ zonedTimeToLocalTime zt
    (year, doy) = toOrdinalDate d
    leap = isLeapYear year
    (moy, dom) = dayOfYearToMonthAndDay leap doy
    dow = DT.dayOfWeek d
    (sec, picosec) = second resolution $ divMod' (todSec t) 1
    tz = zonedTimeZone zt
