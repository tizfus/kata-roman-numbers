module Lib
    ( fromNumber
    ) where

fromNumber :: Int -> String
fromNumber number =
    let 
        (romanSymbol, decimalNumber) = takeEgualOrLowerRomanNumber number
        reminder = number - decimalNumber
    in romanSymbol ++ 
        if isZero reminder then [] else fromNumber reminder

takeEgualOrLowerRomanNumber :: Int -> (String, Int)
takeEgualOrLowerRomanNumber number =
    head 
    $ filter ((number >=) . snd ) 
    $ romanNumbers

isZero :: Int -> Bool
isZero = (== 0)

romanNumbers :: [(String, Int)]
romanNumbers = [
    (drawOneThousands 4, 4000),
    (drawOneThousand, 1000),
    (drawOneHundred ++ drawOneThousand, 900),
    (drawFiveHundred, 500),
    (drawOneHundred ++ drawFiveHundred, 400),
    (drawOneHundred, 100),
    (drawTen ++ drawOneHundred, 90),
    (drawFifty, 50),
    (drawTen ++ drawFifty, 40),
    (drawTen, 10),
    (drawOne ++ drawTen, 9),
    (drawFive, 5),
    (drawOne ++ drawFive, 4),
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