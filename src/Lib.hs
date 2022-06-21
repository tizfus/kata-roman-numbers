module Lib
    ( fromNumber
    ) where

fromNumber :: Int -> String
fromNumber 1 = drawOne
fromNumber 5 = drawFive
fromNumber 10 = drawTen
fromNumber 50 = drawFifty
fromNumber 100 = drawOneHundred
fromNumber 500 = drawFiveHundred
fromNumber 1000 = drawOneThousand

fromNumber 4 = drawOne ++ drawFive
fromNumber 40 = drawTen ++ drawFifty
fromNumber 400 = drawOneHundred ++ drawFiveHundred
fromNumber 4000 = drawOneThousands 4

fromNumber 9 = drawOne ++ drawTen
fromNumber 90 = drawTen ++ drawOneHundred
fromNumber 900 = drawOneHundred ++ drawOneThousand

fromNumber number
    | (number > 1000) = drawOneThousand ++ (fromNumber $ number - 1000)
    | (number > 500) = drawFiveHundred ++ (fromNumber $ number - 500)
    | (number > 100) = drawOneHundred ++ (fromNumber $ number - 100)
    | (number > 50) = drawFifty ++ (fromNumber $ number - 50)
    | (number > 10) = drawTen ++ (fromNumber $ number - 10)
    | (number > 5) = drawFive ++ drawOnes (number - 5)
    | otherwise = drawOnes number

drawOne = "I"
drawOnes = draws drawOne

drawFive = "V"

drawTen = "X"
drawTens = draws drawTen

drawFifty = "L"

drawFiveHundred = "D"

drawOneHundred = "C"

drawOneThousand = "M"
drawOneThousands = draws drawOneThousand

draws :: String -> Int -> String 
draws letter amount = concat $ take amount $ repeat letter 