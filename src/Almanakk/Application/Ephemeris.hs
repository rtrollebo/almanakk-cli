module Almanakk.Application.Ephemeris (
    sunRiseSetTzMain
) where

import Data.Time.Clock
import Data.Time.LocalTime
import Almanakk.Almanakk
import Almanakk.Application.View
import Almanakk.Application.AppContext
import Almanakk.Application.Calendar.Internal
import Almanakk.Ephemeris.Sun


sunRiseSetTzMain :: UTCTime -> Double -> Double -> Maybe Int -> IO()
sunRiseSetTzMain t lat lon tzInt = do
    let sr = sunRisingSetting t Rising lon lat
    let ss = sunRisingSetting t Setting lon lat
    let deltaRise = sunRisingSettingDailyDelta t lon lat Rising
    let deltaSet = sunRisingSettingDailyDelta t lon lat Setting 
    tzSystem <- getTimeZone t
    let tz = case tzInt of 
                (Nothing) -> tzSystem
                (Just tza) -> hoursToTimeZone tza
    composeResultEphemeris t lat lon tz sr ss deltaRise deltaSet 

composeResultEphemeris :: UTCTime 
    -> Double -> Double -> TimeZone 
    -> Either AppContext (Maybe UTCTime) 
    -> Either AppContext (Maybe UTCTime) 
    -> Maybe NominalDiffTime -> Maybe NominalDiffTime 
    -> IO()
composeResultEphemeris t lat lon tz sr ss deltaRise deltaSet = 
    putStr 
        (
            "\nObserver data\n" ++
            (toStr [
                ("Date", utcTimeToDayStr t), 
                ("Time zone", show tz),
                ("Location", "lat " ++ show lat ++ " lon " ++ show lon)]) ++
            "\nSolar celestial rising and setting\n" ++
            (toStr [
                ("Sunrise", processResult tz sr),
                ("Sunset", processResult tz ss),
                ("Δ rise", processDeltaRiseResult deltaRise),
                ("Δ set", processDeltaRiseResult deltaSet)])
        )


processResult :: TimeZone -> Either AppContext (Maybe UTCTime ) -> String
processResult tz (Right (Just value)) = localTimeToString (utcToLocalTime tz value)
processResult _ (Right Nothing) = "-" -- Does not rise or set
processResult _ (Left err)  = getContext err

processDeltaRiseResult :: Maybe NominalDiffTime -> String
processDeltaRiseResult Nothing = "-"
processDeltaRiseResult (Just deltaRise) = show deltaRise