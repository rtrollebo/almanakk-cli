module Main (main) where 
{- 
Almanakk Main module

-}

import System.Environment   
import Control.Exception
import Data.List (sort)
import Data.Time.Clock
import Data.Time.LocalTime
import Options.Applicative
import Almanakk.Ephemeris.Sun (sunRisingSetting, sunRisingSettingDailyDelta)
import Almanakk.Phase.Moon
import Almanakk.Application.AppContext (AppContext)
import Almanakk.Application.Version
import Almanakk.Application.View (processResult, processDeltaRiseResult, toStr)
import Almanakk.Application.Phase
import Almanakk.Application.Calendar (calendarMainFromTime)
import Almanakk.Application.Calendar.Internal
import Almanakk.Almanakk


data AlmanakkArgs = EphArg Double Double String | PhaseArg String | CalendarArg String

main :: IO ()
main = do
    p <- execParser (info (parser <**> helper <**> (simpleVersioner getVersion)) ( fullDesc <> progDesc "Almanakk CLI - astronomical calendar and ephemeris.\nrtrollebo@gmail.com (C) 2024 " <> header "Almanakk CLI" ) )
    processParser p

processParser :: AlmanakkArgs -> IO ()
processParser (EphArg lat lon tz) = catch (sunRiseSetTzMain lat lon (timezoneFromDefaultValue tz)) handler
processParser (PhaseArg tz) = catch (phaseMain  (timezoneFromDefaultValue tz)) handler
processParser (CalendarArg tz) = catch (calendarMain) handler
processParser _ = return ()

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

parserEphemeris :: Parser AlmanakkArgs
parserEphemeris = EphArg
    <$> argument auto (metavar "lat" <> help "latitude") 
    <*> argument auto (metavar "lon" <> help "latitude")
    <*> strOption (long "timezone" <> metavar "timezone" <> help "Specify the timezone" <> showDefault <> value "0")

parserPhase :: Parser AlmanakkArgs
parserPhase = PhaseArg
    <$> strOption (long "timezone" <> metavar "timezone" <> help "Specify the timezone" <> showDefault <> value "0")

parserCalendar :: Parser AlmanakkArgs
parserCalendar = CalendarArg
    <$> strOption (long "timezone" <> metavar "timezone" <> help "Specify the timezone" <> showDefault <> value "0")

addInfo :: Parser a -> String -> ParserInfo a
addInfo p d = info (helper <*> p) $ progDesc d

parser :: Parser AlmanakkArgs
parser = subparser
       ( command "ephemeris" (addInfo parserEphemeris  "Calculate the ephemeris at the specified latitude and longitude." )
      <> command "phase" (addInfo parserPhase  "Calculate the lunar phase. ")
      <> command "calendar" (addInfo parserCalendar  "Calculate the astronomical calendar. "))

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

timezoneFromDefaultValue :: String -> Maybe Int
timezoneFromDefaultValue tz
    | tz == "0" = Nothing
    | otherwise = Just (read tz :: Int)

handler :: SomeException -> IO ()
handler _ = putStrLn $ "Unexpected error occured.\n"

