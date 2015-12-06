module MathService
    ( hammingDistance,
      euclideanDistance,
      arithmeticMean
    ) where

import Data.Vector as V
import Statistics.Sample
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


arithmeticMean :: [Double] -> Double
arithmeticMean list = Prelude.foldr (+) 0 list / Prelude.foldr (\x y -> 1+y) 0 list
