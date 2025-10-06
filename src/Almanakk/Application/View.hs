module Almanakk.Application.View (
    toStr
    , toBlock
    , localTimeToString
    , localDateToString
    , localTimeFloor
    , cellestialPhaseEvent2
    , AlmanakkEventEntry(..)
) where

import Data.Time
import Data.Time(utctDay)
import Almanakk.Application.Phase.Internal



data AlmanakkEventEntry = AlmanakkEventEntry { 
    entryTime2 :: LocalTime,
    cellestialObject2 :: String,
    cellestialPhaseEvent2 :: LunarPhaseEvent } deriving (Show) 

instance Ord AlmanakkEventEntry where 
    compare x y = compare (entryTime2 x) (entryTime2 y)

instance Eq AlmanakkEventEntry where
    x == y = (entryTime2 x) ==  (entryTime2 y)  
    x /= y = (entryTime2 x) /=  (entryTime2 y) 

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
