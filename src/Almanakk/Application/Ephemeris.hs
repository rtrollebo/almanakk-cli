module Almanakk.Application.Ephemeris (
    sunRiseSetTzMain
) where

import Data.Time.Clock
import Data.Time.LocalTime
import Almanakk.Application.View
import Almanakk.Application.Calendar.Internal
import Almanakk.Application.External


sunRiseSetTzMain :: UTCTime -> Double -> Double -> Maybe Int -> IO()
sunRiseSetTzMain t lat lon tzInt = do
    let sr = sunRise t lat lon
    let ss = sunSet t lat lon
    let deltaRise = sunRiseDelta t lat lon
    let deltaSet = sunSetDelta t lat lon
    tzSystem <- getTimeZone t
    let tz = case tzInt of 
                (Nothing) -> tzSystem
                (Just tza) -> hoursToTimeZone tza
    composeResultEphemeris t lat lon tz sr ss deltaRise deltaSet 


composeResultEphemeris :: UTCTime 
    -> Double -> Double -> TimeZone 
    -> Either String (Maybe UTCTime) 
    -> Either String (Maybe UTCTime) 
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

processResult :: TimeZone -> Either String (Maybe UTCTime ) -> String
processResult tz (Right (Just value)) = localTimeToString (utcToLocalTime tz value)
processResult _ (Right Nothing) = "-" -- Does not rise or set
processResult _ (Left err)  = err

processDeltaRiseResult :: Maybe NominalDiffTime -> String
processDeltaRiseResult Nothing = "-"
processDeltaRiseResult (Just deltaRise) = show deltaRise