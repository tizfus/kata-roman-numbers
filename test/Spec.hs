import Control.Monad(forM_)
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
            let numbers = [
                    ("I", 1)
                    , ("II", 2)
                    , ("III", 3)
                    , ("IV", 4)
                    , ("V", 5)
                    , ("VI", 6)
                    , ("VII", 7)
                    , ("VIII", 8)
                    , ("IX", 9)
                    , ("X", 10)
                    , ("XI", 11)
                    , ("XII", 12)
                    , ("XIII", 13)
                    , ("XIV", 14)
                    , ("XV", 15)
                    , ("XVI", 16)
                    , ("XVII", 17)
                    , ("XVIII", 18)
                    , ("XIX", 19)
                    , ("XX", 20)
                    ]
            forM_ numbers (\(expected, number) -> assertSymbol expected $ fromNumber number)

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