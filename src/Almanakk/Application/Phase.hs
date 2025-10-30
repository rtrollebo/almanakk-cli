module Almanakk.Application.Phase (
    celPhaseResultToAee
    , cellestialPhaseToStr
    , aeeToStr
) where

import Data.Time
import Almanakk.Application.View
import Almanakk.Application.Phase.Internal 


celPhaseResultToAee :: TimeZone -> [(LunarPhaseEvent, UTCTime)] -> [AlmanakkEventEntry] 
celPhaseResultToAee _ [] = [] 
celPhaseResultToAee tz (x:xs) = case x of
    (cpe, eventTime) -> [AlmanakkEventEntry (utcToLocalTime tz eventTime) "Moon" cpe] ++ celPhaseResultToAee tz xs

cellestialPhaseToStr :: LunarPhase -> String
cellestialPhaseToStr cph = show cph


aeeToStr :: [AlmanakkEventEntry] -> String
aeeToStr [] = ""
aeeToStr (AlmanakkEventEntry lt co cpe:xs) = 
    "  " ++ toBlock (localDateToString lt) ++ 
    "  " ++ toBlock co ++ 
    "  " ++ toBlock (show cpe) ++ "\n" ++ (aeeToStr xs )
