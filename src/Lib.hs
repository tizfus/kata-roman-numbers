module Lib
    ( fromNumber
    ) where

fromNumber :: Int -> String
fromNumber 1 = [drawI]
fromNumber 5 = [drawV]
fromNumber 10 = [drawX]
fromNumber 50 = "L"
fromNumber 100 = "C"
fromNumber 500 = "D"
fromNumber 1000 = "M"

fromNumber 4 = fromNumber 1 ++ fromNumber 5
fromNumber 9 = fromNumber 1 ++ fromNumber 10
fromNumber 19 = fromNumber 1 ++ fromNumber 20

fromNumber number
    | number == 40 = "XL"
    | (number > 1) && (number < 5) = drawIs number
    | (number > 5) && (number < 10) = drawV : drawIs (number - 5)
    | (number > 10) = drawX : (fromNumber $ number - 10)
    | otherwise = "WRONG_VALUE"

drawI = 'I'
drawIs amount = take amount $ repeat 'I'
drawV = 'V'
drawX = 'X'
drawXs amount = take amount $ repeat 'X'

