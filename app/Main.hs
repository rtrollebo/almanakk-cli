module Main (main) where 
{- 
Almanakk Main module

-}

import System.Environment   
import Control.Exception
import Data.List (sort)
import Data.Time.Clock
import Data.Time.LocalTime
import Almanakk.Ephemeris.Sun (sunRisingSetting, sunRisingSettingDailyDelta)
import Almanakk.Phase.Moon
import Almanakk.Application.AppContext (AppContext)
import Almanakk.Application.Version
import Almanakk.Application.View (processResult, processDeltaRiseResult, toStr, celPhaseResultToAee, aeeToStr, cellestialPhaseToStr, AlmanacEventEntry)
import Almanakk.Application.Calendar (calendarMainFromTime)
import Almanakk.Almanac

-- Entry point  of almanakk-cli
main :: IO ()
main = mainAlmanac

mainAlmanac :: IO ()
mainAlmanac = do 
    args <- getArgs
    case args of
        ["ephemeris", lat, lon] -> catch (sunRiseSetTzMain (read lat :: Double) (read lon :: Double) (Nothing)) handler
        ["ephemeris", lat, lon, tz] -> catch (sunRiseSetTzMain (read lat :: Double) (read lon :: Double) (Just (read tz :: Int))) handler
        ["phase", tz] -> catch (phaseMain  (Just (read tz :: Int))) handler
        ["phase"] -> catch (phaseMain  Nothing) handler
        ["calendar"] -> catch (calendarMain) handler
        ["--version"] -> putStrLn $ (getVersion)
        _             -> putStrLn $ "almanakk " 
            ++ getVersion ++ "\nrtrollebo@gmail.com (C) 2024 "
            ++ "\nUsage:" 
            ++ "\nalmanakk ephemeris <latitude> <longitude> [time_zone]"
            ++ "\nalmanakk phase [time_zone]"
            ++ "\nalmanakk calendar"

calendarMain :: IO()
calendarMain = do 
    t <- getCurrentTime
    calendarMainFromTime t

phaseMain :: Maybe Int -> IO()
phaseMain tzInt = do
    t <- getCurrentTime
    tzsystem <- getTimeZone t
    let moonPh = MoonPhase t
    let tz = case tzInt of 
                (Nothing) -> tzsystem
                (Just tza) -> hoursToTimeZone tza
    let aeeList = celPhaseResultToAee tz [(New, (new moonPh)), (FirstQuarter, (firstQuarter moonPh)), (Full, (full moonPh)), (LastQuarter, (lastQuarter moonPh))]
    let phse = phase moonPh
    composeResultPhase (sort aeeList) [("Lunar phase",  cellestialPhaseToStr phse)]

sunRiseSetTzMain :: Double -> Double -> Maybe Int -> IO()
sunRiseSetTzMain lat lon tzInt = do
    t <- getCurrentTime
    tzsystem <- getTimeZone t
    let sr = sunRisingSetting t Rising lon lat
    let ss = sunRisingSetting t Setting lon lat
    let deltaRise = sunRisingSettingDailyDelta t lon lat Rising
    let deltaSet = sunRisingSettingDailyDelta t lon lat Setting 
    let tz = case tzInt of 
                (Nothing) -> tzsystem
                (Just tza) -> hoursToTimeZone tza
    let moonPh = MoonPhase t
    let aeeList = celPhaseResultToAee tz [(New, (new moonPh)), (FirstQuarter, (firstQuarter moonPh)), (Full, (full moonPh)), (LastQuarter, (lastQuarter moonPh))]
    let phse = phase moonPh
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

composeResultPhase :: [AlmanacEventEntry]
    -> [(String, String)]
    -> IO()
composeResultPhase aee phse = 
    putStr 
        (
            "\nCurrent celestial phases\n" ++
            (toStr phse) ++ 
            "\nNext celestial phase events\n" ++
            (aeeToStr aee
            )
        )

handler :: SomeException -> IO ()
handler _ = putStrLn $ "Unexpected error occured.\n"

