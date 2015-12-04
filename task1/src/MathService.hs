module MathService
    ( hammingDistance,
      euclideanDistance,
      normalize_matrix
    ) where

-- import Data.Matrix
-- import qualified Data.Vector    as V


hammingDistance :: [Double] -> [Double] -> Int
hammingDistance listX listY = sum $ zipWith diff listX listY
                where diff a b | a == b = 0
                               | otherwise = 1

euclideanDistance :: [Double] -> [Double] -> Double
euclideanDistance listX listY = sqrt $ sum $ zipWith diff listX listY
                where diff a b | a == b = 0
                               | otherwise = 1


generate_random_centers :: Int -> [[Double]] -> [[Double]]
generate_random_centers count o = take count o


-- generate_random_matrix :: Int -> Int -> IO [[Double]]
-- generate_random_matrix co cl = do
--     rand <- generate_random_string co cl
--     return $ normalize_matrix (chunksOf cl rand)


normalize_matrix :: [[Double]] -> [[Double]]
normalize_matrix a = map (\ row -> map (/ (sum row)) row) a


-- generate_random_string :: Int -> Int -> IO [Double]
-- generate_random_string co cl = do
--     stdGen <- newStdGen
--     return $ take (co * cl) (randoms stdGen :: [Double])
