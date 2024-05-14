module Almanakk.Application.Calendar (
    calendarMainFromTime
) where

import Data.Time
import Data.List (sort)
import Almanakk.Calendar.Calendar (dateOfEaster)
import Almanakk.Application.View
import Almanakk.Orbit.Equinox
import Almanakk.Almanac

-- instance CalendarUnit UTCTime
--     addUTCTimeEventToCalendar :: UTCTime -> String -> [CalendarEntry] -> [CalendarEntry]

data CalendarEntry = CalendarEntry {
    -- Basic calendar entry for the standard Christian Calendar and astronomical events
    -- Temporary type to contain data from newly exposed functions from almanakk-lib
    -- (to be combined with AlmanacEventEntry later)
    calendarEntryName :: String,
    calendarEntryDay :: Day } deriving (Show)


class CalendarUnit a where
    addEventToCalendar :: a -> String -> [CalendarEntry] -> [CalendarEntry]


addUTCTimeEventToCalendar :: UTCTime -> String -> [CalendarEntry] -> [CalendarEntry]
addUTCTimeEventToCalendar (UTCTime d s) name entries = entries ++ [(CalendarEntry name d)]

addDayEventToCalendar :: Day -> String -> [CalendarEntry] -> [CalendarEntry]
addDayEventToCalendar d name entries = entries ++ [(CalendarEntry name d)]

instance CalendarUnit UTCTime where
    addEventToCalendar = addUTCTimeEventToCalendar

instance CalendarUnit Day where
    addEventToCalendar = addDayEventToCalendar

instance Ord AlmanacEventEntry where 
    compare x y = compare (entryTime x) (entryTime y)

instance Eq AlmanacEventEntry where
    x == y = (entryTime x) ==  (entryTime y)  
    x /= y = (entryTime x) /=  (entryTime y)  

instance Ord CalendarEntry where
    compare x y = compare (calendarEntryDay x) (calendarEntryDay y)

instance Eq CalendarEntry where
    x == y = (calendarEntryDay x) == (calendarEntryDay y)


calendarMainFromTime :: UTCTime -> IO()
calendarMainFromTime t = do 

    -- Date of easter
    doeList <- getDateOfEasterList t

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

    -- Compose list
    let equinoxNorthwardList = case eqnNorth of 
                        (Left ctx) -> doeList
                        (Right v) -> addEventToCalendar v ("Equinox northward "++(localTimeToString (utcToLocalTime tzsystem v)))  doeList
    let calendarList = case eqnSouth of 
                        (Left ctx) -> doeList
                        (Right v) -> addEventToCalendar v ("Equinox southward "++(localTimeToString (utcToLocalTime tzsystem v))) equinoxNorthwardList
    let calEntriesStr = calendarEntriesToStr $ sort $ (getCalendarEntriesFiltered t calendarList)
    putStr "Christian holidays and astronomical events\n\n" -- For now, show holidays based on day of easter. Additional holiday entries to be added later. 
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
    | otherwise = [CalendarEntry "Transfiguration Sunday" (addDays (-7*7) easterday)]
                ++ [CalendarEntry "Ash Wednesday" (addDays (-46) easterday)]
                ++ [CalendarEntry "Palm Sunday" (addDays (-7) easterday)]
                ++ [CalendarEntry "Maundy Thursday" (addDays (-3) easterday)]
                ++ [CalendarEntry "Good Friday" (addDays (-2) easterday)]
                ++ easterEntry
                ++ [CalendarEntry "Divine Mercy Sunday" (addDays (7) easterday)]
                ++ [CalendarEntry "Ascension of Jesus" (addDays (39) easterday)]
                ++ [CalendarEntry "Pentecost" (addDays (7*7) easterday)] 
                ++ [CalendarEntry "Trinity Sunday" (addDays ((7*7)+7) easterday)]
                where easterday = calendarEntryDay $ head easterEntry

getCalendarEntriesFiltered :: UTCTime -> [CalendarEntry] -> [CalendarEntry]
getCalendarEntriesFiltered (UTCTime d _) calEntryInitial = filter (\x -> (calendarEntryDay x)>d) calEntries
    where calEntries = getCalendarEntries calEntryInitial

calendarEntriesToStr :: [CalendarEntry] -> String
calendarEntriesToStr [] = ""
calendarEntriesToStr (CalendarEntry n d:xs) = "  " ++ toBlock n ++ "  " ++ toBlock (showDay d) ++ "\n" ++ calendarEntriesToStr xs
    where showDay :: Day -> String
          showDay day = show day
