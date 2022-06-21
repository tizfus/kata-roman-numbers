module Lib
    ( fromNumber
    ) where

fromNumber :: Int -> String
fromNumber 1 = [drawI]
fromNumber 5 = [drawV]
fromNumber 10 = [drawX]
fromNumber 50 = [drawL]
fromNumber 100 = "C"
fromNumber 500 = [drawD]
fromNumber 1000 = [drawM]

fromNumber 4 = [drawI, drawV]
fromNumber 40 = [drawI, drawL]
fromNumber 400 = [drawI, drawD]
fromNumber 4000 = drawMs 4

fromNumber 9 = [drawI , drawX]
fromNumber 90 = [drawI , drawC]
fromNumber 900 = [drawI , drawM]

fromNumber 19 = fromNumber 1 ++ fromNumber 20

fromNumber number
    | (number > 1) && (number < 5) = drawIs number
    | (number > 5) && (number < 10) = drawV : drawIs (number - 5)
    | (number > 10) = drawX : (fromNumber $ number - 10)
    | otherwise = "WRONG_VALUE"

drawI = 'I'
drawIs = draws drawI

drawV = 'V'

drawX = 'X'
drawXs = draws drawX

drawL = 'L'

drawD = 'D'

drawC = 'C'

drawM = 'M'
drawMs = draws drawM

draws :: Char -> Int -> String 
draws letter amount = take amount $ repeat letter 