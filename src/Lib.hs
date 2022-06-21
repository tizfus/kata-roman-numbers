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

fromNumber number =
    let 
        numeral = takeMinorNumeral number
        reminder = number - numeral
    in fromNumber numeral ++ 
        if isZero reminder then [] else fromNumber reminder

takeMinorNumeral :: Int -> Int
takeMinorNumeral number =
    let romanNumeral = [1000, 500, 100, 50, 10, 5, 1]
    in
        head $ filter (number > ) romanNumeral

isZero :: Int -> Bool
isZero = (== 0)

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