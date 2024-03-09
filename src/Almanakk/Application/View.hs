module Almanakk.Application.View (
    processDeltaRiseResult
    , processLunarPhaseResult
    , processResult
    , toStr
    , AlmanacEventEntry (..)
    , celPhaseResultToAee
    , aeeToStr
    , getCalendarEntries 
    , calendarEntriesToStr 
    , cellestialPhaseToStr
) where

import Data.Time
import Almanakk.Application.AppContext
import Almanakk.Almanac
import Almanakk.Calendar.Calendar (dateOfEaster, dateOfPentecost)


data AlmanacEventEntry = AlmanacEventEntry { 
    entryTime :: LocalTime,
    cellestialObject :: CelestialObject,
    cellestialPhaseEvent :: CellestialPhaseEvent } deriving (Show) 

data CalendarEntry = CalendarEntry {
    -- Basic calendar entry for the standard Christian Calendar
    -- Temporary type to contain data from newly exposed functions from almanakk-lib
    -- (to be combined with AlmanacEventEntry later)
    calendarEntryName :: String,
    calendarDay :: Maybe Day } deriving (Show)

instance Ord AlmanacEventEntry where 
    compare x y = compare (entryTime x) (entryTime y)

instance Eq AlmanacEventEntry where
    x == y = (entryTime x) ==  (entryTime y)  
    x /= y = (entryTime x) /=  (entryTime y)  

cellestialPhaseToStr :: CellestialPhase -> String
cellestialPhaseToStr cph = show cph

getCalendarEntries :: UTCTime -> [CalendarEntry]
getCalendarEntries t = [CalendarEntry "Easter" dateEaster] ++ [CalendarEntry "Pentecost" datePentecost]
    where dateEaster = case (dateOfEaster t) of
                     -- dateOfEaster calculates the day of easter from the year in t
                     (Left _) -> Nothing
                     (Right d) -> d
          datePentecost = case (dateOfPentecost t) of
                     -- dateOfPentecost calculates the day of pentecost from the year in t
                     (Left _) -> Nothing
                     (Right d) -> d

calendarEntriesToStr :: [CalendarEntry] -> String
calendarEntriesToStr [] = ""
calendarEntriesToStr (CalendarEntry n d:xs) = "  " ++ toBlock n ++ "  " ++ toBlock (showDay d) ++ "\n" ++ (calendarEntriesToStr xs)
    where showDay :: Maybe Day -> String
          showDay Nothing = ""
          showDay (Just d) = show d


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
processLunarPhaseResult tz Nothing = "-"

processDeltaRiseResult :: Maybe NominalDiffTime -> String
processDeltaRiseResult Nothing = "-"
processDeltaRiseResult (Just deltaRise) = show deltaRise

localTimeToString :: LocalTime -> String
localTimeToString (LocalTime d (TimeOfDay todHour todMin todSec)) = show todHour ++ "h" ++ show (floorToNearest todMin 5) ++ "m"

localDateToString :: LocalTime -> String
localDateToString (LocalTime d (TimeOfDay todHour todMin todSec)) = 
    -- In this publicly available version, round calculation results
    -- from the almanakk-lib library to nearest 5 minutes. 
    gregStr ++ show todHour ++ "h" ++ show (floorToNearest todMin 5) ++ "m"
    where 
        gregStr = case toGregorian d of
            (y, m, d) -> show y ++ "-" ++ show m ++ "-" ++ show d ++ " "

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
