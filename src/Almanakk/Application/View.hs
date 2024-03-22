module Almanakk.Application.View (
    processDeltaRiseResult
    , processLunarPhaseResult
    , processResult
    , toStr
    , AlmanacEventEntry (..)
    , celPhaseResultToAee
    , aeeToStr 
    , getCalendarEntriesFiltered
    , calendarEntriesToStr 
    , cellestialPhaseToStr
) where




import Data.Time
import Data.Time(utctDay)
import Almanakk.Application.AppContext
import Almanakk.Almanac
import Almanakk.Calendar.Calendar (dateOfEaster)



data AlmanacEventEntry = AlmanacEventEntry { 
    entryTime :: LocalTime,
    cellestialObject :: CelestialObject,
    cellestialPhaseEvent :: CellestialPhaseEvent } deriving (Show) 

data CalendarEntry = CalendarEntry {
    -- Basic calendar entry for the standard Christian Calendar
    -- Temporary type to contain data from newly exposed functions from almanakk-lib
    -- (to be combined with AlmanacEventEntry later)
    calendarEntryName :: String,
    calendarEntryDay :: Day } deriving (Show)

instance Ord AlmanacEventEntry where 
    compare x y = compare (entryTime x) (entryTime y)

instance Eq AlmanacEventEntry where
    x == y = (entryTime x) ==  (entryTime y)  
    x /= y = (entryTime x) /=  (entryTime y)  

instance Ord CalendarEntry where
    compare x y = compare (calendarEntryDay x) (calendarEntryDay y)

instance Eq CalendarEntry where
    x == y = (calendarEntryDay x) == (calendarEntryDay y)

cellestialPhaseToStr :: CellestialPhase -> String
cellestialPhaseToStr cph = show cph

getCalendarEntries :: UTCTime -> [CalendarEntry]
getCalendarEntries t = entriesAll
    where doe = case (dateOfEaster t) of
              -- dateOfEaster calculates the day of easter from the year in t
              (Left _) -> Nothing
              (Right d) -> d
          easterEntry = case doe of 
                  (Just d) -> [CalendarEntry "Easter Sunday" d]
                  (Nothing) -> []
          entriesAll 
            | (length easterEntry) == 0 = []
            | otherwise = 
                [CalendarEntry "Transfiguration Sunday" (addDays (-7*7) easterday)]
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

getCalendarEntriesFiltered :: UTCTime  -> [CalendarEntry]
getCalendarEntriesFiltered (UTCTime d s) = filter (\x -> (calendarEntryDay x)>d) calEntries
    where calEntries = getCalendarEntries (UTCTime d s)

calendarEntriesToStr :: [CalendarEntry] -> String
calendarEntriesToStr [] = ""
calendarEntriesToStr (CalendarEntry n d:xs) = "  " ++ toBlock n ++ "  " ++ toBlock (showDay d) ++ "\n" ++ calendarEntriesToStr xs
    where showDay :: Day -> String
          showDay day = show day


aeeToStr :: [AlmanacEventEntry] -> String
aeeToStr [] = ""
aeeToStr (AlmanacEventEntry lt co cpe:xs) = 
    "  " ++ toBlock (localDateToString lt) ++ 
    "  " ++ toBlock (show co) ++ 
    "  " ++ toBlock (show cpe) ++ "\n" ++ (aeeToStr xs )

celPhaseResultToAee :: TimeZone -> [(CellestialPhaseEvent, UTCTime)] -> [AlmanacEventEntry]
celPhaseResultToAee _ [] = []
celPhaseResultToAee tz (x:xs) = case x of
    (cpe, eventTime) -> [AlmanacEventEntry (utcToLocalTime tz eventTime) Moon cpe] ++ celPhaseResultToAee tz xs


processResult :: TimeZone -> Either AppContext (Maybe UTCTime ) -> String
processResult tz (Right (Just value)) = localTimeToString (utcToLocalTime tz value)
processResult _ (Right Nothing) = "-" -- Does not rise or set
processResult _ (Left err)  = getContext err

processLunarPhaseResult :: TimeZone -> Maybe UTCTime -> String
processLunarPhaseResult tz (Just value) = localDateToString (utcToLocalTime tz value)
processLunarPhaseResult _ Nothing = "-"

processDeltaRiseResult :: Maybe NominalDiffTime -> String
processDeltaRiseResult Nothing = "-"
processDeltaRiseResult (Just deltaRise) = show deltaRise

localTimeToString :: LocalTime -> String
localTimeToString (LocalTime _ (TimeOfDay h m _)) = show h ++ "h" ++ show (floorToNearest m 5) ++ "m"

localDateToString :: LocalTime -> String
localDateToString (LocalTime d (TimeOfDay h m _)) = 
    -- In this publicly available version, round calculation results
    -- from the almanakk-lib library to nearest 5 minutes. 
    gregStr ++ show h ++ "h" ++ show (floorToNearest m 5) ++ "m"
    where 
        gregStr = case toGregorian d of
            (year, month, date) -> show year ++ "-" ++ show month ++ "-" ++ show date ++ " "

-- Floor x to nearest r
floorToNearest :: Int -> Int -> Int
floorToNearest x r = (floor ((fromIntegral x)/(fromIntegral r))*r)

toStr :: [(String, String)] -> String
toStr [] = ""
toStr ((c1,c2):xs) = "  " ++ toBlock c1 ++ "  " ++ toBlock c2 ++ "\n" ++ (toStr xs )

toBlock :: String -> String
toBlock x
    | length x < width = x ++ replicate (width - (length x)) ' '
    | otherwise = take width x
        where width = 20
