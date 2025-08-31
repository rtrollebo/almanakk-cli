import Test.HUnit

testApp = TestCase $ assertEqual "placeholder test" 0 0

tests = TestList [TestLabel "placeholder test" testApp]

main :: IO ()
main = do
  runTestTT tests
  return ()