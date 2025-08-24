{- | 
Module with mock definitions of external library functions.
For testing purpose.  
 -}
module Almanakk.Application.External.Mock
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
import Almanakk.Application.Calendar.Internal
import Almanakk.Application.Phase.Internal 

sunRise :: UTCTime -> Double -> Double -> Either String (Maybe UTCTime)
sunRise _ _ _ = Right Nothing

sunSet :: UTCTime -> Double -> Double -> Either String (Maybe UTCTime)
sunSet _ _ _ = Right Nothing

sunRiseDelta :: UTCTime -> Double -> Double -> Maybe NominalDiffTime
sunRiseDelta _ _ _ = Nothing

sunSetDelta :: UTCTime -> Double -> Double -> Maybe NominalDiffTime
sunSetDelta _ _ _ = Nothing

easter :: UTCTime -> Either String (Maybe Day)
easter _ = Right Nothing

planetPerihelion :: Planet -> UTCTime -> Either String UTCTime
planetPerihelion PlanetMercury _ = Right (UTCTime (fromGregorian 2025 08 01) (secondsToDiffTime 0))
planetPerihelion _ _ = Left "Not implemented"

planetAphelion :: Planet -> UTCTime -> Either String UTCTime
planetAphelion PlanetMercury _ = Right (UTCTime (fromGregorian 2025 08 01) (secondsToDiffTime 0))
planetAphelion _ _ = Left "Not implemented"

solsticeNorthern :: Int -> Either String UTCTime
solsticeNorthern _ = Right (UTCTime (fromGregorian 2025 08 01) (secondsToDiffTime 0))

solsticeSouthern :: Int -> Either String UTCTime
solsticeSouthern _ = Right (UTCTime (fromGregorian 2025 08 01) (secondsToDiffTime 0))

equinoxNorthward :: Int -> Either String UTCTime
equinoxNorthward _ = Right (UTCTime (fromGregorian 2025 08 01) (secondsToDiffTime 0))

equinoxSouthward :: Int -> Either String UTCTime
equinoxSouthward _ = Right (UTCTime (fromGregorian 2025 08 01) (secondsToDiffTime 0))

lunarPhaseNew :: UTCTime -> UTCTime
lunarPhaseNew _ = UTCTime (fromGregorian 2025 08 01) (secondsToDiffTime 0)

lunarPhaseFirstQuarter :: UTCTime -> UTCTime
lunarPhaseFirstQuarter _ = UTCTime (fromGregorian 2025 08 01) (secondsToDiffTime 0)

lunarPhaseFull :: UTCTime -> UTCTime
lunarPhaseFull _ = UTCTime (fromGregorian 2025 08 01) (secondsToDiffTime 0)

lunarPhaseLastQuarter :: UTCTime -> UTCTime
lunarPhaseLastQuarter _ = UTCTime (fromGregorian 2025 08 01) (secondsToDiffTime 0)

lunarPhase :: UTCTime -> LunarPhase
lunarPhase _ = LunarWaningCrescent
