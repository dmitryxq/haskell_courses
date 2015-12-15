module MathService
    ( arithmeticMean,
      varianceList,
      meanAndDispersian,
      pvc,
      pC,
      attrCount,
      trainedCount
    ) where

import qualified Data.Map as M
import Types

arithmeticMean :: [Double] -> Double
arithmeticMean list = foldr (+) 0 list / foldr (\x y -> 1+y) 0 list

varianceList :: [Double] -> Double
varianceList list = dispNumerator / dispDevider
    where
        dispNumerator = sum  $ map (\x -> (x - arithmeticMean list) ** 2)  list
        dispDevider = fromIntegral $ (length list - 1)


meanAndDispersian :: [Double] -> (Double, Double)
meanAndDispersian list = (arithmeticMean list, varianceList list)

pvc :: Dictionary -> Double -> Int -> Double
pvc dictionary xi index = numerator / denominator
                                where (mean, disp) = meanAndDispersian (dictionary M.! index)
                                      numerator = exp ((xi - mean) ^ 2) / (-2 * disp)
                                      denominator = sqrt (2.0 * pi * disp) 

pC :: TrainedData -> String -> Double 
pC trained className = classCount / totalCount
                        where classCount = fromIntegral $ attrCount $ trained M.! className
                              totalCount = fromIntegral $ trainedCount trained

attrCount :: Dictionary -> Int
attrCount = M.fold (\attrs count -> count + (length attrs)) 0

trainedCount :: TrainedData -> Int
trainedCount = M.fold (\classData count -> count + (attrCount classData)) 0
