module Lib
    ( fromNumber
    ) where

import Data.List (sortBy)
import Data.Function (on)

fromNumber :: Int -> String
fromNumber 0 = []
fromNumber number =
    let 
        (romanSymbol, decimalNumber) = takeEgualOrLowerRomanNumber number
        reminder = number - decimalNumber
    in romanSymbol ++ fromNumber reminder

takeEgualOrLowerRomanNumber :: Int -> (String, Int)
takeEgualOrLowerRomanNumber number =
    head 
    $ filter ((number >=) . snd ) 
    $ romanNumbers

isZero :: Int -> Bool
isZero = (== 0)

romanNumbers :: [(String, Int)]
romanNumbers = 
    sort $ symbols ++ specialNumbers
    where 
        symbols = [
            ("MMMM", 4000),
            ("M", 1000),
            ("D", 500),
            ("C", 100),
            ("L", 50),
            ("X", 10),
            ("V", 5),
            ("I", 1)
            ]

        specialNumbers = [
            ("CM", 900),
            ("CD", 400),
            ("XC", 90),
            ("XL", 40),
            ("IX", 9),
            ("IV", 4)
            ]

sort :: [(String, Int)] -> [(String, Int)]
sort = sortBy (flip compare `on` snd)
