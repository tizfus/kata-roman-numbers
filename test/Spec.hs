import Lib(fromNumber)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, Assertion)

main :: IO ()
main = defaultMain $ testGroup "Roman numbers tests" $ [
        testCase "primitive symbols" $ do
            assertSymbol "I" $ fromNumber 1
            assertSymbol "V" $ fromNumber 5
            assertSymbol "X" $ fromNumber 10
            assertSymbol "L" $ fromNumber 50
            assertSymbol "C" $ fromNumber 100
            assertSymbol "D" $ fromNumber 500
            assertSymbol "M" $ fromNumber 1000

        , testCase "count to 10" $ do
            assertSymbol "I" $ fromNumber 1
            assertSymbol "II" $ fromNumber 2
            assertSymbol "III" $ fromNumber 3
            assertSymbol "IV" $ fromNumber 4
            assertSymbol "V" $ fromNumber 5
            assertSymbol "VI" $ fromNumber 6
            assertSymbol "VII" $ fromNumber 7
            assertSymbol "VIII" $ fromNumber 8
            assertSymbol "IV" $ fromNumber 9
            assertSymbol "X" $ fromNumber 10
            

    ]


assertSymbol :: String -> String -> Assertion
assertSymbol expected actual = 
    assertEqual
        ("wrong symbol! expected: " ++ expected ++ "; found: " ++ actual)
        expected
        actual