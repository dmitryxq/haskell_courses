module MathService
    ( hammingDistance,
      euclideanDistance,
      arithmeticMean
    ) where

import Data.Vector as V
import Statistics.Sample
type Vector = [Double]
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

varianceList :: [Double] -> Double
varianceList list = dispNumerator / dispDevider
	where
		dispNumerator = Prelude.sum  $ Prelude.map (\x -> (x - arithmeticMean list) ** 2)  list
		dispDevider = fromIntegral $ (Prelude.length list - 1)


meanAndDispersian :: [Double] -> (Double, Double)
meanAndDispersian list = (arithmeticMean list, varianceList list)

-- probabilityPVC :: [Vector] -> Vector -> Double -> Double
-- probabilityPVC matrix vectorXIC xi = 1 / (sqrt sigmaSqrDoubled * pi) * exp ( (xi - mean vectorXIC)^2 / sigma * (-1))
--     where 
--         sigma = 2 * dispersion xi matrix


-- dispersion :: Double -> [Vector] -> Double
-- dispersion xi xs = 1 / (n - 1) * foldl (\acc x -> (xi - mean x)**2 + acc) 0 xs
--     where 
--         n = fromIntegral $ length xs