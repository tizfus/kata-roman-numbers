module Lib
    ( fromNumber
    ) where

fromNumber :: Int -> String
fromNumber 1 = "I"
fromNumber 5 = "V"
fromNumber 10 = "X"
fromNumber 50 = "L"
fromNumber 100 = "C"
fromNumber 500 = "D"
fromNumber 1000 = "M"

fromNumber 4 = fromNumber 1 ++ fromNumber 5
fromNumber 9 = fromNumber 1 ++ fromNumber 10
fromNumber 19 = fromNumber 1 ++ fromNumber 20

fromNumber number
    | (number > 1) && (number < 5) = fromNumber 1 ++ (fromNumber $ number - 1)
    | (number > 5) && (number < 10) = fromNumber 5 ++ (fromNumber $ number - 5)
    | (number > 10) = fromNumber 10 ++ (fromNumber $ number - 10)
    | otherwise = "WRONG_VALUE"



