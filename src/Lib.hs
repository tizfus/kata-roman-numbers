module Lib
    ( fromNumber
    ) where

import Data.List (sortBy)
import Data.Function (on)

data RomanNumber = RomanNumber  { symbols :: String, decimal :: Int }

fromNumber :: Int -> String
fromNumber 0 = []
fromNumber number =
    let 
        romanNumber = takeEqualOrLowerRomanNumber number
        reminder = number - decimal romanNumber
    in symbols romanNumber ++ fromNumber reminder

takeEqualOrLowerRomanNumber :: Int -> RomanNumber
takeEqualOrLowerRomanNumber number =
    head 
    $ filter (isEqualOrLower number) 
    $ romanNumbers

isEqualOrLower :: Int -> RomanNumber -> Bool
isEqualOrLower number = (number >=) . decimal

romanNumbers :: [RomanNumber]
romanNumbers = 
    sort $ symbols ++ specialNumbers
    where 
        symbols = [
            RomanNumber "MMMM" 4000,
            RomanNumber "M" 1000,
            RomanNumber "D" 500,
            RomanNumber "C" 100,
            RomanNumber "L" 50,
            RomanNumber "X" 10,
            RomanNumber "V" 5,
            RomanNumber "I" 1
            ]

        specialNumbers = [
            RomanNumber "CM" 900,
            RomanNumber "CD" 400,
            RomanNumber "XC" 90,
            RomanNumber "XL" 40,
            RomanNumber "IX" 9,
            RomanNumber "IV" 4
            ]

sort :: [RomanNumber] -> [RomanNumber]
sort = sortBy (flip compare `on` decimal)
