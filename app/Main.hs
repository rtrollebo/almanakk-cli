module Main (main) where 
{- 
Almanakk Main module

-}

import System.Environment   
import Control.Exception
import Data.List (sort)
import Data.Time.Clock
import Data.Time.LocalTime
import Text.Read
import Options.Applicative
import Almanakk.Application.Version
import Almanakk.Application.View 
import Almanakk.Application.Ephemeris
import Almanakk.Application.Phase
import Almanakk.Application.Calendar (calendarMainFromTime)
import Almanakk.Application.External
import Almanakk.Application.Phase.Internal 


data AlmanakkArgs = EphArg Double Double String | PhaseArg String | CalendarArg String

main :: IO ()
main = do
    p <- execParser (info (parser <**> helper <**> (simpleVersioner getVersion)) ( fullDesc <> progDesc "Almanakk CLI - astronomical calendar and ephemeris.\nrtrollebo@gmail.com (C) 2024 " <> header "Almanakk CLI" ) )
    processParser p

processParser :: AlmanakkArgs -> IO ()
processParser (EphArg lat lon tz) = catch (ephemerisMain lat lon (readMaybe tz)) handler
processParser (PhaseArg tz) = catch (phaseMain  (readMaybe tz)) handler
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
    let tz = case tzInt of 
                (Nothing) -> tzsystem
                (Just tza) -> hoursToTimeZone tza
    let aeeList = celPhaseResultToAee tz [(NewMoon, (lunarPhaseNew t)), (FirstQuarterMoon, (lunarPhaseFirstQuarter t)), (FullMoon, (lunarPhaseFull t)), (LastQuarterMoon, (lunarPhaseLastQuarter t))]
    let phse = lunarPhase t
    composeResultPhase (sort aeeList) [("Lunar phase",  cellestialPhaseToStr phse)]

ephemerisMain :: Double -> Double -> Maybe Int -> IO()
ephemerisMain lat lon tzInt = do
    t <- getCurrentTime
    sunRiseSetTzMain t lat lon tzInt

parserEphemeris :: Parser AlmanakkArgs
parserEphemeris = EphArg
    <$> argument auto (metavar "lat" <> help "latitude") 
    <*> argument auto (metavar "lon" <> help "longitude")
    <*> strOption (long "timezone" <> metavar "timezone" <> help "Specify the timezone" <> showDefault <> value "default")

parserPhase :: Parser AlmanakkArgs
parserPhase = PhaseArg
    <$> strOption (long "timezone" <> metavar "timezone" <> help "Specify the timezone" <> showDefault <> value "default")

parserCalendar :: Parser AlmanakkArgs
parserCalendar = CalendarArg
    <$> strOption (long "timezone" <> metavar "timezone" <> help "Specify the timezone" <> showDefault <> value "default")

addInfo :: Parser a -> String -> ParserInfo a
addInfo p d = info (helper <*> p) $ progDesc d

parser :: Parser AlmanakkArgs
parser = subparser
       ( command "ephemeris" (addInfo parserEphemeris  "Calculate the ephemeris at the specified latitude and longitude." )
      <> command "phase" (addInfo parserPhase  "Calculate the lunar phase. ")
      <> command "calendar" (addInfo parserCalendar  "Calculate the astronomical calendar. "))

composeResultPhase :: [AlmanakkEventEntry]
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

