import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

main :: IO ()
main = defaultMain $ testGroup "Roman numbers tests" $ [
        testCase "green test" (assertEqual "should be green," 1 1)
    ]
