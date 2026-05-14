module Almanakk.Application.Calendar.Internal (
    utcTimeToDayStr
    , Planet(..)
    , currentYear
    , dstStart
    , dstEnd
    , dst
) where

import Data.Time
import Data.Time.Calendar.WeekDate

data Planet = PlanetMercury | PlanetVenus | PlanetEarth | PlanetMars | PlanetJupiter | PlanetSaturn | PlanetUranus | PlanetNeptune 

utcTimeToDayStr :: UTCTime -> String
utcTimeToDayStr  (UTCTime d _) = case (toGregorian d) of
    (y, m, _) -> show y ++ "-" ++ show m ++ "-" ++ show d

currentYear :: UTCTime -> Integer
currentYear t = case t of 
                (UTCTime d _) -> case (toGregorian d) of 
                                 (year, _, _) -> year


dst :: UTCTime -> Bool
dst (UTCTime d dt) 
    | d >= start && d < end = True 
    | otherwise = False 
    where 
        start = dstStart (UTCTime d dt) 
        end = dstEnd (UTCTime d dt) 

dstStart :: UTCTime -> Day
dstStart t = dstBoundary t 3 31

dstEnd :: UTCTime -> Day
dstEnd t = dstBoundary t 10 31

dstBoundary :: UTCTime -> Int -> Int -> Day
dstBoundary t month day = addDays offset lastDay
    where 
        lastDay = fromGregorian (currentYear t) month day
        dow = dayOfWeek lastDay
        offset = dayOfWeekOffset dow

dayOfWeekOffset :: DayOfWeek -> Integer
dayOfWeekOffset Sunday = 0
dayOfWeekOffset Monday = -1
dayOfWeekOffset Tuesday = -2
dayOfWeekOffset Wednesday = -3
dayOfWeekOffset Thursday = -4
dayOfWeekOffset Friday  = -5
dayOfWeekOffset Saturday = -6