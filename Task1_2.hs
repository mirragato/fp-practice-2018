module Task1_2 where

import Todo(todo)
import Prelude hiding (gcd, pow)

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = todo

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = todo

-- наибольший общий делитель двух чисел
gcd :: Integer -> Integer -> Integer
gcd x y
        | x < 0 || y < 0 = error "Error: one of the arguments is less then zero"
        | x == 0 = y
        | y == 0 = x
        | otherwise = gcd y (mod x y)

-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to = (floorSqrt (to - 1) - ceilingSqrt from) >= 0
  where
    floorSqrt = floor . sqrt . fromIntegral
    ceilingSqrt = ceiling . sqrt . fromIntegral

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year
        | day `elem` [1 .. [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] !! (fromIntegral month - 1)] = True
        | isLeapFebruary year month && day == 29 = True
        | otherwise = False
  where
    isLeapYear year = (year `rem` 4 == 0 && year `rem` 100 /= 0) || (year `rem` 400 == 0)
    isLeapFebruary year month = month == 2 && isLeapYear year

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x y
        | y == 0 = 1
        | y == 1 = x
        | otherwise = x * pow x (y - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x
        | x < 0 = error "Error: one of the arguments is less then zero"
        | x == 0 || x == 1 = True
        | otherwise = isPrime' x 2

isPrime' :: Integer -> Integer -> Bool
isPrime' x div
        | x == div = True
        | mod x div == 0 = False
        | otherwise = isPrime' x (div + 1)

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = todo

-- треугольник задан своими координатами.
-- функция должна вернуть 
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c = todo
