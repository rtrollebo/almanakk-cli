module Almanakk.Application.Calendar (
    calendarMainFromTime
) where

import Data.Time
import Data.List (sort)
import Data.Time.Calendar
import Almanakk.Calendar (dateOfEaster)
import Almanakk.Application.View
import Almanakk.Orbit.Equinox
import Almanakk.Orbit.Solstice
import Almanakk.Almanakk


data CalendarEntry = 
    CalendarEntry {
    -- Basic calendar entry for the standard Christian Calendar and astronomical events
    -- Temporary type to contain data from newly exposed functions from almanakk-lib
    -- (to be combined with AlmanacEventEntry later)
        calendarEntryName :: String,
        calendarEntryDay :: Day } | 
    CalendarEntryTime { 
        calendarEntryName :: String, 
        calendarEntryTime :: UTCTime} deriving (Show)


class CalendarUnit a where
    addEventToCalendar :: a -> String -> [CalendarEntry] -> [CalendarEntry]


addUTCTimeEventToCalendar :: UTCTime -> String -> [CalendarEntry] -> [CalendarEntry]
addUTCTimeEventToCalendar t name entries = entries ++ [(CalendarEntryTime name t)]

addDayEventToCalendar :: Day -> String -> [CalendarEntry] -> [CalendarEntry]
addDayEventToCalendar d name entries = entries ++ [(CalendarEntry name d)]

instance CalendarUnit UTCTime where
    addEventToCalendar = addUTCTimeEventToCalendar

instance CalendarUnit Day where
    addEventToCalendar = addDayEventToCalendar

instance Ord CalendarEntry where
    compare x y = compareCalendarEntries x y


-- TODO: fix the instance of Eq, similar to compare
instance Eq CalendarEntry where
    x == y = (calendarEntryDay x) == (calendarEntryDay y)
    x == y = (calendarEntryTime x) == (calendarEntryTime y)

compareCalendarEntries :: CalendarEntry -> CalendarEntry -> Ordering
compareCalendarEntries (CalendarEntry _ day1) (CalendarEntry _ day2) = compare day1 day2
compareCalendarEntries (CalendarEntryTime _ utctime1) (CalendarEntryTime _ utctime2) = compare utctime1 utctime2
compareCalendarEntries (CalendarEntry _ day1) (CalendarEntryTime _ (UTCTime day2 _)) =  compare day1 day2
compareCalendarEntries (CalendarEntryTime _ (UTCTime day1 _)) (CalendarEntry _ day2) = compare day1 day2



calendarMainFromTime :: UTCTime -> IO()
calendarMainFromTime t = do 

    -- Date of easter
    doeList <- getDateOfEasterList t

    -- Add liturgical calendar days based on the date of easter
    let calendarEntriesLiturg = getCalendarEntries doeList

    {-- 
    Equinox of the current year:
    equinox signature:
    equinox :: Equinox -> Int ->  Either AppContext UTCTime
    -}
    tzsystem <- getTimeZone t
    let currentYear = case t of 
                      (UTCTime d _) -> case (toGregorian d) of 
                                       (year, _, _) -> fromIntegral year
    let eqnNorth = equinox Northward currentYear
    let eqnSouth = equinox Southward currentYear

    {-- 
    Solstice of the current year:
    solstice signature:
    solstice :: Solstice -> Int ->  Either AppContext UTCTime
    -}
    let solNorthern = solstice Northern currentYear
    let solSouthern = solstice Southern currentYear

    -- Add astronomical days
    let equinoxNorthwardList = case eqnNorth of 
                        (Left ctx) -> doeList
                        (Right v) -> addEventToCalendar v ("Equinox northward "++(localTimeToString $ utcToLocalTime tzsystem v)) calendarEntriesLiturg
    let equinoxSouthwardList = case eqnSouth of 
                        (Left ctx) -> equinoxNorthwardList
                        (Right v) -> addEventToCalendar v ("Equinox southward "++(localTimeToString $ utcToLocalTime tzsystem v)) equinoxNorthwardList
    let solsticeNorthernList = case solNorthern of 
                        (Left ctx) -> equinoxSouthwardList
                        (Right v) -> addEventToCalendar v ("Solstice northern "++(localTimeToString $ utcToLocalTime tzsystem v)) equinoxSouthwardList
    let calendarList = case solSouthern of 
                        (Left ctx) -> solsticeNorthernList
                        (Right v) -> addEventToCalendar v ("Solstice southern "++(localTimeToString $ utcToLocalTime tzsystem v)) solsticeNorthernList
    let calEntriesStr = calendarEntriesToStr tzsystem $ sort $ getCalendarEntriesFiltered t calendarList
    putStr "Christian holidays and astronomical events\n\n" 
    putStr calEntriesStr

getDateOfEasterList :: UTCTime -> IO [CalendarEntry]
getDateOfEasterList t = return (getDateOfEaster t)


getDateOfEaster :: UTCTime -> [CalendarEntry]
getDateOfEaster t = easterEntry
    where doe = case (dateOfEaster t) of
              -- dateOfEaster calculates the day of easter from the year in t
              (Left _) -> Nothing
              (Right d) -> d
          easterEntry = case doe of 
                  (Just d) -> [CalendarEntry "Easter Sunday" d]
                  (Nothing) -> []

getCalendarEntries :: [CalendarEntry] -> [CalendarEntry]
getCalendarEntries easterEntry   
    | (length easterEntry) == 0 = []
    | otherwise = easterEntry
                ++ [CalendarEntry "Transfiguration Sunday" (addDays (-7*7) easterday)]
                ++ [CalendarEntry "Ash Wednesday" (addDays (-46) easterday)]
                ++ [CalendarEntry "Palm Sunday" (addDays (-7) easterday)]
                ++ [CalendarEntry "Maundy Thursday" (addDays (-3) easterday)]
                ++ [CalendarEntry "Good Friday" (addDays (-2) easterday)]
                ++ [CalendarEntry "Divine Mercy Sunday" (addDays (7) easterday)]
                ++ [CalendarEntry "Ascension of Jesus" (addDays (39) easterday)]
                ++ [CalendarEntry "Pentecost" (addDays (7*7) easterday)] 
                ++ [CalendarEntry "Trinity Sunday" (addDays ((7*7)+7) easterday)]
                where easterday = calendarEntryDay $ head easterEntry

getCalendarEntriesFiltered :: UTCTime -> [CalendarEntry] -> [CalendarEntry]
getCalendarEntriesFiltered t calEntryInitial = filter (\x -> (compareCalendarEntryWithUtcTime t x)) calEntryInitial

compareCalendarEntryWithUtcTime :: UTCTime -> CalendarEntry -> Bool
compareCalendarEntryWithUtcTime (UTCTime d s) entry =  case entry of
                                                       (CalendarEntry _ day) -> day > d
                                                       (CalendarEntryTime _ utctime) ->  utctime > (UTCTime d s)

calendarEntriesToStr :: TimeZone -> [CalendarEntry] -> String
calendarEntriesToStr _ [] = ""
calendarEntriesToStr tz (CalendarEntryTime n utctime:xs) = "  " ++ toBlock (show localDay) ++ "  " ++ toBlock n ++ "  " ++ "\n" ++ calendarEntriesToStr tz xs
    where localDay = case (utcToLocalTime tz utctime) of 
                     (LocalTime d _) -> d
calendarEntriesToStr tz (CalendarEntry n d:xs) = "  " ++ toBlock (showDay d) ++ "  " ++ toBlock n ++ "  " ++ "\n" ++ calendarEntriesToStr tz xs
    where showDay :: Day -> String
          showDay day = show day

