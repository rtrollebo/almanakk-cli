import Test.HUnit

import Data.List (sort)
import Data.Time
import Almanakk.Application.View
    ( roundToTwoDecimals,
      AlmanakkEventEntry(AlmanakkEventEntry, cellestialPhaseEvent) )
import Almanakk.Application.Phase.Internal
import Almanakk.Application.Calendar.Internal


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
testDstStart1 = TestCase $ assertEqual "test DST start 1" (dstStart (UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0))) (fromGregorian 2026 3 29)
testDstStart2 = TestCase $ assertEqual "test DST start 1" (dstStart (UTCTime (fromGregorian 2005 1 1) (secondsToDiffTime 0))) (fromGregorian 2005 3 27)
testDst1 = TestCase $ assertEqual "test DST 1" (dst (UTCTime (fromGregorian 2005 1 1) (secondsToDiffTime 0))) False
testDst2 = TestCase $ assertEqual "test DST 2" (dst (UTCTime (fromGregorian 2009 10 24) (secondsToDiffTime 0))) True
testDst3 = TestCase $ assertEqual "test DST 3" (dst (UTCTime (fromGregorian 2009 10 25) (secondsToDiffTime 0))) False
testDst4 = TestCase $ assertEqual "test DST 4" (dst (UTCTime (fromGregorian 2026 3 28) (secondsToDiffTime 0))) False
testDst5 = TestCase $ assertEqual "test DST 5" (dst (UTCTime (fromGregorian 2026 3 29) (secondsToDiffTime 0))) True
testDst6 = TestCase $ assertEqual "test DST 6" (dst (UTCTime (fromGregorian 2026 10 24) (secondsToDiffTime 0))) True
testDst7 = TestCase $ assertEqual "test DST 7" (dst (UTCTime (fromGregorian 2026 10 25) (secondsToDiffTime 0))) False
-- 29 25


tests = TestList [
  TestLabel "AlmanakkEventEntry sort test" testApp, 
  TestLabel "test rounding 1" testRounding1,
  TestLabel "test rounding 2" testRounding2,
  TestLabel "test rounding 3" testRounding3,
  TestLabel "test DST start 1" testDstStart1,
  TestLabel "test DST start 2" testDstStart2,
  TestLabel "test DST 1" testDst1,
  TestLabel "test DST 2" testDst2,
  TestLabel "test DST 3" testDst3,
  TestLabel "test DST 4" testDst4,
  TestLabel "test DST 5" testDst5,
  TestLabel "test DST 6" testDst6,
  TestLabel "test DST 7" testDst7]

main :: IO ()
main = do
  runTestTT tests
  return ()