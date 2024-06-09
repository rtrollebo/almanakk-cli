module Almanakk.Application.Phase (
    celPhaseResultToAee
    , cellestialPhaseToStr
    , AlmanacEventEntry (..)
    , aeeToStr 
) where

import Data.Time
import Data.Time(utctDay)
import Almanakk.Application.AppContext
import Almanakk.Almanac
import Almanakk.Application.View


data AlmanacEventEntry = AlmanacEventEntry { 
    entryTime :: LocalTime,
    cellestialObject :: CelestialObject,
    cellestialPhaseEvent :: CellestialPhaseEvent } deriving (Show) 

instance Ord AlmanacEventEntry where 
    compare x y = compare (entryTime x) (entryTime y)

instance Eq AlmanacEventEntry where
    x == y = (entryTime x) ==  (entryTime y)  
    x /= y = (entryTime x) /=  (entryTime y)  

celPhaseResultToAee :: TimeZone -> [(CellestialPhaseEvent, UTCTime)] -> [AlmanacEventEntry]
celPhaseResultToAee _ [] = []
celPhaseResultToAee tz (x:xs) = case x of
    (cpe, eventTime) -> [AlmanacEventEntry (utcToLocalTime tz eventTime) Moon cpe] ++ celPhaseResultToAee tz xs

cellestialPhaseToStr :: CellestialPhase -> String
cellestialPhaseToStr cph = show cph

aeeToStr :: [AlmanacEventEntry] -> String
aeeToStr [] = ""
aeeToStr (AlmanacEventEntry lt co cpe:xs) = 
    "  " ++ toBlock (localDateToString lt) ++ 
    "  " ++ toBlock (show co) ++ 
    "  " ++ toBlock (show cpe) ++ "\n" ++ (aeeToStr xs )
