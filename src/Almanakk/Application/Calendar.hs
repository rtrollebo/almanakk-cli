module Almanakk.Application.Calendar (
    calendarMainFromTime
) where

import Data.Time
import Data.List (sort)
import Data.Time.Calendar
import Almanakk.Application.View
import Almanakk.Application.External
import Almanakk.Application.Calendar.Internal

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

data CalendarCollection = CalendarCollection {
    calendarCollection :: [CalendarEntry],
    calendarEvent :: (Either String UTCTime, String),
    ccTimeZone :: TimeZone}

calendarMainFromTime :: UTCTime -> IO()
calendarMainFromTime t = do 

    -- Date of easter
    doeList <- getDateOfEasterList t

    -- Add liturgical calendar days based on the date of easter

    -- System time zone
    tzsystem <- getTimeZone t

    -- Current year
    let cy = fromIntegral $ currentYear t

    -- Add astronomical days
    let collections = [
            {-- 
            Equinox of the current year:
            equinox signature:
            equinox :: Equinox -> Int ->  Either AppContext UTCTime
            -}
            CalendarCollection [] (equinoxNorthward cy, "Equinox northward ") tzsystem,
            CalendarCollection [] (equinoxSouthward cy, "Equinox southward ") tzsystem, 
            {-- 
            Solstice of the current year:
            solstice signature:
            solstice :: Solstice -> Int ->  Either AppContext UTCTime
            -}
            CalendarCollection [] (solsticeNorthern cy, "Solstice northern ") tzsystem, 
            CalendarCollection [] (solsticeSouthern cy, "Solstice southern ") tzsystem,             

            CalendarCollection [] (planetPerihelion PlanetMercury t, "Perihelion, Mercury ") tzsystem,
            CalendarCollection [] (planetPerihelion PlanetVenus t, "Perihelion, Venus ") tzsystem,
            CalendarCollection [] (planetPerihelion PlanetEarth t, "Perihelion, Earth ") tzsystem,
            CalendarCollection [] (planetPerihelion PlanetMars t, "Perihelion, Mars ") tzsystem,
            CalendarCollection [] (planetPerihelion PlanetJupiter t, "Perihelion, Jupiter ") tzsystem,
            CalendarCollection [] (planetPerihelion PlanetSaturn t, "Perihelion, Saturn ") tzsystem,
            CalendarCollection [] (planetPerihelion PlanetUranus t, "Perihelion, Uranus ") tzsystem,
            CalendarCollection [] (planetPerihelion PlanetNeptune t, "Perihelion, Neptune ") tzsystem,
            CalendarCollection [] (planetAphelion PlanetMercury t, "Aphelion, Mercury ") tzsystem,
            CalendarCollection [] (planetAphelion PlanetVenus t, "Aphelion, Venus ") tzsystem,
            CalendarCollection [] (planetAphelion PlanetEarth t, "Aphelion, Earth ") tzsystem,
            CalendarCollection [] (planetAphelion PlanetMars t, "Aphelion, Mars ") tzsystem,
            CalendarCollection [] (planetAphelion PlanetJupiter t, "Aphelion, Jupiter ") tzsystem,
            CalendarCollection [] (planetAphelion PlanetSaturn t, "Aphelion, Saturn ") tzsystem,
            CalendarCollection [] (planetAphelion PlanetUranus t, "Aphelion, Uranus ") tzsystem,
            CalendarCollection [] (planetAphelion PlanetNeptune t, "Aphelion, Neptune ") tzsystem]



    let collectionResult = foldl 
            joinCalendarCollection 
            (CalendarCollection (getCalendarEntries doeList) (Left ("_"), "") tzsystem) 
            collections

    let calEntriesStr = calendarEntriesToStr tzsystem $ sort $ getCalendarEntriesFiltered t (calendarCollection collectionResult)
    putStr "Christian holidays and astronomical events\n\n" 
    putStr calEntriesStr

joinCalendarCollection :: CalendarCollection -> CalendarCollection -> CalendarCollection
joinCalendarCollection cc1 cc2 = CalendarCollection value (ce, descr) (ccTimeZone cc2)
    where value = case ce of
                (Left ctx) -> calendarCollection cc2
                (Right v) -> addEventToCalendar v (descr++(localTimeToString $ utcToLocalTime tzsystem v)) (calendarCollection cc1)
          ce = fst (calendarEvent cc2)
          descr = snd (calendarEvent cc2)
          tzsystem = ccTimeZone cc2

getDateOfEasterList :: UTCTime -> IO [CalendarEntry]
getDateOfEasterList t = return (getDateOfEaster t)


getDateOfEaster :: UTCTime -> [CalendarEntry]
getDateOfEaster t = easterEntry
    where doe = case (easter t) of
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
                                                       (CalendarEntryTime _ utctime) ->  (utctime > (UTCTime d s)) && ((utctime < (UTCTime (fromGregorian (currentYear (UTCTime d s)) 12 31) (secondsToDiffTime 0)))) 

calendarEntriesToStr :: TimeZone -> [CalendarEntry] -> String
calendarEntriesToStr _ [] = ""
calendarEntriesToStr tz (CalendarEntryTime n utctime:xs) = "  " ++ toBlock (show localTimeDay) ++ "  " ++ toBlock n ++ "  " ++ "\n" ++ calendarEntriesToStr tz xs
    where localTimeDay = case (utcToLocalTime tz utctime) of 
                     (LocalTime d _) -> d
calendarEntriesToStr tz (CalendarEntry n d:xs) = "  " ++ toBlock (showDay d) ++ "  " ++ toBlock n ++ "  " ++ "\n" ++ calendarEntriesToStr tz xs
    where showDay :: Day -> String
          showDay day = show day

currentYear :: UTCTime -> Integer
currentYear t = case t of 
                (UTCTime d _) -> case (toGregorian d) of 
                                 (year, _, _) -> fromIntegral year
