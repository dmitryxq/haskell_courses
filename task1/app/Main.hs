module Main where

import System.Environment
import MathService
import ParserService
import ParserArgumentParams
import Types
import System.IO
import System.IO.Error
import Pipes
import Data.Csv
import Data.Char
import Data.Vector as V
import Statistics.Sample
import Control.Exception
-- import Data.Text (Text)
import Data.ByteString.Lazy
import qualified Data.ByteString.Lazy as BL

import Data.List.Split
import BayesClassifier

-- | File for tested
csvFile :: FilePath
csvFile = "resources/glass.txt"

sample = fromList [1.5,1.3,1.2,1.9,2.1]

main :: IO ()
main = do
    -- System.IO.putStrLn "Ololo"
    -- -- print $ arithmeticMean [1.5,1.3,1.2,1.9,2.1]
    -- -- print $ variance sample
    -- -- print $ mean sample
    -- -- print $ getMeanAndVarianceList sample
    -- System.IO.putStrLn "============================================================================"
    options <- getArgs >>= parseInputArguments
    -- contents <- BL.readFile csvFile
    readResult <- try (BL.readFile csvFile):: IO(Either SomeException ByteString)
    case readResult of
         Left someException -> print someException
         Right contents -> do
             let result = parserCSVFile contents options
             print result
             -- putStrLn result
    System.IO.putStrLn "fine"
    -- MAIN function main

getMeanAndVarianceList :: V.Vector Double -> V.Vector Double
getMeanAndVarianceList list = fromList [meanList, varianceList]
    where meanList = mean list
          varianceList = variance list
    

    -- hSetBuffering stdout NoBuffering
    -- hSetBuffering stdin NoBuffering
    -- interactionLoop classifier "start" 
    -- testConduit


-- interactionLoop myClassifier function = case function of 
--                                           "start" ->  
--                                             do
--                                               System.IO.putStrLn "Enter an action [train|classify]"
--                                               action <- getLine
--                                               interactionLoop myClassifier action
--                                           "train" -> 
--                                             do
--                                               System.IO.putStr "Category: "
--                                               category <- getLine
--                                               System.IO.putStr "Material: "
--                                               material <- getLine
--                                               interactionLoop (train myClassifier material category) "start"
--                                           "classify" ->
--                                             do
--                                               System.IO.putStr "Material: "
--                                               material <- getLine
--                                               System.IO.putStrLn $ classify myClassifier material
--                                               System.IO.putStrLn . show $ probabilities myClassifier material
--                                               System.IO.putStrLn "\n\n\n\n"
--                                               interactionLoop myClassifier "start"
--                                           _ ->
--                                               interactionLoop myClassifier "start"

    -- putStrLn "Please input parameters: "
    -- putStrLn $ show $ splitOn "," "1,2,3,4"
 --    arguments <- getLine
    
 --    let fileName = arguments !! 0
    -- -- test
 --    putStrLn fileName


-- test hammingDistance

-- main = putStrLn $ show $ hammingDistance [1, 2, 3] [2, 3, 1, 5]

-- test input arg     
-- main = do
-- 	putStrLn "Please, write input.csv file:"
-- 	csvFile <- getLine
-- 	putStrLn "Count clasters:"
-- 	countClaster <- getLine
-- 	putStrLn "Write epsilon:"
-- 	epsilon <- getLine
-- 	putStrLn "Thanks"


