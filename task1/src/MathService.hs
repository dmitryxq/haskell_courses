module MathService
    ( hammingDistance,
      euclideanDistance
    ) where

import Data.Vector as V
-- hammingDistance :: [Double] -> [Double] -> Int
-- hammingDistance listX listY = sum $ zipWith diff listX listY
--                 where diff a b | a == b = 0
--                                | otherwise = 1

-- euclideanDistance :: [Double] -> [Double] -> Double
-- euclideanDistance listX listY = sqrt $ sum $ zipWith diff listX listY
--                 where diff a b | a == b = 0
--                                | otherwise = 1


hammingDistance :: V.Vector Double -> V.Vector Double -> Double
hammingDistance listX listY = V.sum $ V.map abs diff
  where diff = V.zipWith (-) listY listY

euclideanDistance :: V.Vector Double -> V.Vector Double -> Double
euclideanDistance listX listY = sqrt $ V.sum $ V.map abs diff
  where diff = V.zipWith (-) listY listY
