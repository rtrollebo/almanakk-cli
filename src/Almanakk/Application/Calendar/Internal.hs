module Almanakk.Application.Calendar.Internal (
    utcTimeToDayStr, Planet(..)
) where

import Data.Time

data Planet = PlanetMercury | PlanetVenus | PlanetEarth | PlanetMars | PlanetJupiter | PlanetSaturn | PlanetUranus | PlanetNeptune 

utcTimeToDayStr :: UTCTime -> String
utcTimeToDayStr  (UTCTime d _) = case (toGregorian d) of
    (y, m, d) -> show y ++ "-" ++ show m ++ "-" ++ show d