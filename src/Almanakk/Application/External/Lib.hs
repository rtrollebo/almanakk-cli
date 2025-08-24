{- | 
The interface to the external library. 
The purpose of this module is to convert external types into internal types, 
to avoid dependence of external types outside this module, 
 -}
module Almanakk.Application.External.Lib
(
    sunRise, 
    sunSet,
    sunRiseDelta,
    sunSetDelta,
    easter,
    solsticeNorthern,
    solsticeSouthern,
    planetPerihelion,
    planetAphelion,
    equinoxNorthward,
    equinoxSouthward,
    lunarPhaseNew,
    lunarPhaseFirstQuarter,
    lunarPhaseFull,
    lunarPhaseLastQuarter,
    lunarPhase
) where 

import Data.Time.Clock
import Data.Time.Calendar
import Almanakk.Ephemeris.Sun
import Almanakk.Application.AppContext
import Almanakk.Almanakk
import Almanakk.Calendar (dateOfEaster)
import Almanakk.Application.Calendar.Internal
import Almanakk.Orbit.Apsis
import Almanakk.CellestialSystem 
import Almanakk.Orbit.Solstice
import Almanakk.Orbit.Equinox
import Almanakk.Application.Phase.Internal
import Almanakk.Phase.Moon

sunRise :: UTCTime -> Double -> Double -> Either String (Maybe UTCTime)
sunRise t lat lon  = case (sunRisingSetting t Rising lon lat) of
    (Left appctx) -> Left (getContext appctx)
    (Right Nothing) -> Right Nothing 
    (Right (Just timeOfSunRise)) -> Right (Just timeOfSunRise)

sunSet :: UTCTime -> Double -> Double -> Either String (Maybe UTCTime)
sunSet t lat lon  = case (sunRisingSetting t Setting lon lat) of
    (Left appctx) -> Left (getContext appctx)
    (Right Nothing) -> Right Nothing 
    (Right (Just timeOfSunSet)) -> Right (Just timeOfSunSet)

sunRiseDelta :: UTCTime -> Double -> Double -> Maybe NominalDiffTime
sunRiseDelta t lat lon = sunRisingSettingDailyDelta t lon lat Rising 

sunSetDelta :: UTCTime -> Double -> Double -> Maybe NominalDiffTime
sunSetDelta t lat lon = sunRisingSettingDailyDelta t lon lat Setting 

easter :: UTCTime -> Either String (Maybe Day)
easter t = case (dateOfEaster t) of 
    (Left apctx) -> Left (getContext apctx)
    (Right d) -> Right d

planetPerihelion :: Planet -> UTCTime -> Either String UTCTime
planetPerihelion p t = case (perihelion (planetTocelestialBodySystem p) t) of
    (Left appctx) -> Left (getContext appctx)
    (Right time) -> Right time

planetAphelion :: Planet -> UTCTime -> Either String UTCTime
planetAphelion p t = case (aphelion (planetTocelestialBodySystem p) t) of
    (Left appctx) -> Left (getContext appctx)
    (Right time) -> Right time


planetTocelestialBodySystem :: Planet -> CelestialBody 
planetTocelestialBodySystem PlanetMercury = CelestialBodySolarSystem Mercury
planetTocelestialBodySystem PlanetVenus = CelestialBodySolarSystem Venus
planetTocelestialBodySystem PlanetEarth = CelestialBodySolarSystem Earth
planetTocelestialBodySystem PlanetMars = CelestialBodySolarSystem Mars
planetTocelestialBodySystem PlanetJupiter = CelestialBodySolarSystem Jupiter
planetTocelestialBodySystem PlanetSaturn = CelestialBodySolarSystem Saturn
planetTocelestialBodySystem PlanetUranus = CelestialBodySolarSystem Uranus
planetTocelestialBodySystem PlanetNeptune = CelestialBodySolarSystem Neptune

solsticeNorthern :: Int -> Either String UTCTime
solsticeNorthern y = case (solstice Northern y) of
    (Left appctx) -> Left (getContext appctx)
    (Right time) -> Right time

solsticeSouthern :: Int -> Either String UTCTime
solsticeSouthern y = case (solstice Southern y) of
    (Left appctx) -> Left (getContext appctx)
    (Right time) -> Right time

equinoxNorthward :: Int -> Either String UTCTime
equinoxNorthward y = case (equinox Northward y) of
    (Left appctx) -> Left (getContext appctx)
    (Right time) -> Right time

equinoxSouthward :: Int -> Either String UTCTime
equinoxSouthward y = case (equinox Southward y) of
    (Left appctx) -> Left (getContext appctx)
    (Right time) -> Right time

lunarPhaseNew :: UTCTime -> UTCTime
lunarPhaseNew t = new $ MoonPhase t

lunarPhaseFirstQuarter :: UTCTime -> UTCTime
lunarPhaseFirstQuarter t = firstQuarter $ MoonPhase t

lunarPhaseFull :: UTCTime -> UTCTime
lunarPhaseFull t = full $ MoonPhase t

lunarPhaseLastQuarter :: UTCTime -> UTCTime
lunarPhaseLastQuarter t = lastQuarter $ MoonPhase t

lunarPhase :: UTCTime -> LunarPhase
lunarPhase t = toLunarPhase $ phase $ MoonPhase t

toLunarPhase :: CellestialPhase -> LunarPhase
toLunarPhase WaxingCrescent = LunarWaxingCrescent
toLunarPhase WaxingGibbous = LunarWaxingGibbous
toLunarPhase WaningGibbous = LunarWaningGibbous 
toLunarPhase WaningCrescent = LunarWaningCrescent
