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
testRounding1 = TestCase $ assertEqual "test rounding 1" (roundToTwoDecimals 0.1234) "0.12"
testRounding2 = TestCase $ assertEqual "test rounding 2" (roundToTwoDecimals 0.1299) "0.13"
testRounding3 = TestCase $ assertEqual "test rounding 3" (roundToTwoDecimals (-0.1299)) "-0.13"

tests = TestList [
  TestLabel "AlmanakkEventEntry sort test" testApp, 
  TestLabel "test rounding 1" testRounding1,
  TestLabel "test rounding 2" testRounding2,
  TestLabel "test rounding 3" testRounding3]

main :: IO ()
main = do
  runTestTT tests
  return ()