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
    ("MMMM", 4000),
    ("M", 1000),
    ("CM", 900),
    ("D", 500),
    ("CD", 400),
    ("C", 100),
    ("XC", 90),
    ("L", 50),
    ("XL", 40),
    ("X", 10),
    ("IX", 9),
    ("V", 5),
    ("IV", 4),
    ("I", 1)
    ]