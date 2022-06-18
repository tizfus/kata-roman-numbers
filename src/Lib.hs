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

fromNumber 4 = "IV"
fromNumber number = take number $ repeat 'I' 


