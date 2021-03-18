module RemoveCycles (removeCycles) where

import Data.Function ((&))

removeCycles :: (Eq a) => [a] -> [a]
removeCycles xs = removeNto0Cycles (length xs `div` 2) xs

removeNto0Cycles :: (Eq a) => Int -> [a] -> [a]
removeNto0Cycles 0 = id
removeNto0Cycles n = removeNto0Cycles (n -1) . removeNCycles n

removeNCycles :: (Eq a) => Int -> [a] -> [a]
removeNCycles n [] = []
removeNCycles n xs =
    if xLeft == xRight
        then removeNCycles n (xLeft ++ remainingXs)
        else head xs : removeNCycles n (tail xs) where
            xLeft = take n xs
            xRight = take n $ drop n xs
            remainingXs = drop (2*n) xs