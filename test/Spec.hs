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

        , testCase "count to 20" $ do
            assertSymbol "I" $ fromNumber 1
            assertSymbol "II" $ fromNumber 2
            assertSymbol "III" $ fromNumber 3
            assertSymbol "IV" $ fromNumber 4
            assertSymbol "V" $ fromNumber 5
            assertSymbol "VI" $ fromNumber 6
            assertSymbol "VII" $ fromNumber 7
            assertSymbol "VIII" $ fromNumber 8
            assertSymbol "IX" $ fromNumber 9
            assertSymbol "X" $ fromNumber 10
            assertSymbol "XI" $ fromNumber 11
            assertSymbol "XII" $ fromNumber 12
            assertSymbol "XIII" $ fromNumber 13
            assertSymbol "XIV" $ fromNumber 14
            assertSymbol "XV" $ fromNumber 15
            assertSymbol "XVI" $ fromNumber 16
            assertSymbol "XVII" $ fromNumber 17
            assertSymbol "XVIII" $ fromNumber 18
            assertSymbol "IXX" $ fromNumber 19
            assertSymbol "XX" $ fromNumber 20

        , testCase "count 4, 40, 400, 4000" $ do
            assertSymbol "IV" $ fromNumber 4
            assertSymbol "IL" $ fromNumber 40
            assertSymbol "ID" $ fromNumber 400
            assertSymbol "MMMM" $ fromNumber 4000

        , testCase "count 9, 90, 900" $ do
            assertSymbol "IX" $ fromNumber 9
            assertSymbol "IC" $ fromNumber 90
            assertSymbol "IM" $ fromNumber 900
    ]


assertSymbol :: String -> String -> Assertion
assertSymbol expected actual = 
    assertEqual
        ("wrong symbol! expected: " ++ expected ++ "; found: " ++ actual)
        expected
        actual