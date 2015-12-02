module MathService
    ( hammingDistance,
      euclideanDistance
    ) where

hammingDistance :: [Double] -> [Double] -> Int
hammingDistance listX listY = sum $ zipWith diff listX listY
                where diff a b | a == b = 0
                               | otherwise = 1

euclideanDistance :: [Double] -> [Double] -> Double
euclideanDistance listX listY = sqrt $ sum $ zipWith diff listX listY
                where diff a b | a == b = 0
                               | otherwise = 1


