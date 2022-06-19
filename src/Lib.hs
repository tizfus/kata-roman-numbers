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

fromNumber 6 = fromNumber 5 ++ fromNumber 1
fromNumber 7 = fromNumber 5 ++ fromNumber 2
fromNumber 8 = fromNumber 5 ++ fromNumber 3

fromNumber 4 = fromNumber 1 ++ fromNumber 5
fromNumber 9 = fromNumber 1 ++ fromNumber 10

fromNumber number = take number $ repeat 'I'



