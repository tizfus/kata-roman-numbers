module Lib
    ( fromNumber
    ) where

fromNumber :: Int -> String
fromNumber 4 = drawOne ++ drawFive
fromNumber 40 = drawTen ++ drawFifty
fromNumber 400 = drawOneHundred ++ drawFiveHundred
fromNumber 4000 = drawOneThousands 4

fromNumber 9 = drawOne ++ drawTen
fromNumber 90 = drawTen ++ drawOneHundred
fromNumber 900 = drawOneHundred ++ drawOneThousand

fromNumber number =
    let 
        (romanSymbol, decimalNumber) = takeMinorNumeral number
        reminder = number - decimalNumber
    in romanSymbol ++ 
        if isZeroOrLess reminder then [] else fromNumber reminder

takeMinorNumeral :: Int -> (String, Int)
takeMinorNumeral number =
        head 
        $ filter ((number >=) . snd ) 
        $ romanNumbers

isZeroOrLess :: Int -> Bool
isZeroOrLess = (<= 0)

romanNumbers :: Num a => [(String, a)]
romanNumbers = [
    (drawOneThousand, 1000),
    (drawFiveHundred, 500),
    (drawOneHundred, 100),
    (drawFifty, 50),
    (drawTen, 10),
    (drawFive, 5),
    (drawOne, 1)
    ]

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