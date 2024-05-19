module Almanakk.Application.View (
    processDeltaRiseResult
    , processLunarPhaseResult
    , processResult
    , toStr
    , toBlock
    , AlmanacEventEntry (..)
    , celPhaseResultToAee
    , aeeToStr 
    , cellestialPhaseToStr
    , localTimeToString
    , localTimeFloor
) where

import Data.Time
import Data.Time(utctDay)
import Almanakk.Application.AppContext
import Almanakk.Almanac


data AlmanacEventEntry = AlmanacEventEntry { 
    entryTime :: LocalTime,
    cellestialObject :: CelestialObject,
    cellestialPhaseEvent :: CellestialPhaseEvent } deriving (Show) 

cellestialPhaseToStr :: CellestialPhase -> String
cellestialPhaseToStr cph = show cph

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

localTimeFloor :: LocalTime -> LocalTime
localTimeFloor (LocalTime d (TimeOfDay h m _)) = (LocalTime d (TimeOfDay h (floorToNearest m 5) 0))


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
        where width = 26
