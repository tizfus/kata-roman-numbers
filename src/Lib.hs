module Lib
    ( fromNumber
    ) where

fromNumber :: Int -> String
fromNumber 1 = drawI
fromNumber 5 = drawV
fromNumber 10 = drawX
fromNumber 50 = drawL
fromNumber 100 = drawC
fromNumber 500 = drawD
fromNumber 1000 = drawM

fromNumber 4 = drawI ++ drawV
fromNumber 40 = drawI ++ drawL
fromNumber 400 = drawI ++ drawD
fromNumber 4000 = drawMs 4

fromNumber 9 = drawI ++ drawX
fromNumber 90 = drawI ++ drawC
fromNumber 900 = drawI ++ drawM

fromNumber number
    | (number > 10) = drawX ++ (fromNumber $ number - 10)
    | (number > 5) = drawV ++ drawIs (number - 5)
    | (number > 1) = drawIs number
    
    | otherwise = "WRONG_VALUE"

drawI = "I"
drawIs = draws drawI

drawV = "V"

drawX = "X"
drawXs = draws drawX

drawL = "L"

drawD = "D"

drawC = "C"

drawM = "M"
drawMs = draws drawM

draws :: String -> Int -> String 
draws letter amount = concat $ take amount $ repeat letter 