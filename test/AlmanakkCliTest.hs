import Test.HUnit

import Data.List (sort)
import Data.Time
import Almanakk.Application.View
import Almanakk.Application.Phase.Internal


utcTime = UTCTime (fromGregorian 2025 10 1) (secondsToDiffTime 0)
timeZone = TimeZone (60) False "Oslo" 


aee = [
  (AlmanakkEventEntry (utcToLocalTime timeZone (UTCTime (fromGregorian 2025 11 20) (secondsToDiffTime 0))) "Moon" LastQuarterMoon),
  (AlmanakkEventEntry (utcToLocalTime timeZone (UTCTime (fromGregorian 2025 11 4) (secondsToDiffTime 0))) "Moon" FullMoon),
  (AlmanakkEventEntry (utcToLocalTime timeZone (UTCTime (fromGregorian 2025 10 1) (secondsToDiffTime 0))) "Moon" NewMoon),
  (AlmanakkEventEntry (utcToLocalTime timeZone (UTCTime (fromGregorian 2025 10 17) (secondsToDiffTime 0))) "Moon" FirstQuarterMoon)
  ]

testApp = TestCase $ assertEqual "AlmanakkEventEntry sort test" (cellestialPhaseEvent $ head  $ sort  aee) NewMoon

tests = TestList [TestLabel "AlmanakkEventEntry sort test" testApp]

main :: IO ()
main = do
  runTestTT tests
  return ()