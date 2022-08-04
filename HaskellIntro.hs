{-# OPTIONS_GHC -fwarn-tabs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module HaskellIntro where

import Set
import System.Win32 (COORD(x), fILE_ADD_FILE)

-- Load this file into GHCi (say, with `ghci HaskellIntro.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

isThisWorking :: String
isThisWorking = "Yes"

--
-- Problem 1
--

lastDigit :: Integer -> Integer
lastDigit num
    | (num < 10) = num
    | otherwise = num `rem` 10


dropLastDigit :: Integer -> Integer
dropLastDigit num
    | (num < 10) = 0
    | otherwise = div num 10

append :: Integer -> [Integer] -> [Integer]
append a [] = [a]
append a (x:xs) = x : append a xs

toDigits :: Integer -> [Integer]
toDigits num
    | (num == 0) = reverse ([])
    | (num < 0) = []
    | otherwise = toDigits (num `div` 10) ++ [lastDigit num]

reverseEveryOther :: [Integer] -> [Integer]
reverseEveryOther [] = []
reverseEveryOther (x:y:xs) = x : (y*2) : reverseEveryOther xs
reverseEveryOther (x:xs) = (x*2): reverseEveryOther xs


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther(x:xs) = reverse (reverseEveryOther (reverse (x:xs)))


sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate x = val (lastDigit (sumDigits (doubleEveryOther (toDigits x))))
    where
        val :: Integer -> Bool
        val x
            |x == 0 = True
            |otherwise = False

--
-- Problem 2
--
square x = x*x

pow :: (a -> a) -> Int -> a -> a
pow f 0 = f
pow f 1 = f
pow f n = f . pow f (n - 1)


g :: Integer -> Integer
g 0 = 0
g n = pow n - ((g . g) n - 1)

h :: Integer -> Integer
h 0 = 0
h n = pow n - ((h . h . h) n - 1)
    --n - ((h . h . h) n - 1)

d :: Int -> Integer -> Integer
d 0 = 0
d i = pow n - (d(n-1)) i

--
-- Problem 3
--

powerSet :: Int => Set a -> Set (Set a)
powerSet x 
    |isEmpty x = empty
    |(size x = 1) = singleton x
    
