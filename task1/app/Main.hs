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
import Control.Exception
-- import Data.Text (Text)
import Data.ByteString.Lazy
import qualified Data.ByteString.Lazy as BL

import Data.List.Split

-- | File for tested
csvFile :: FilePath
csvFile = "resources/glass.txt"

main :: IO ()
main = do
	-- MAIN function main

    -- options <- getArgs >>= parseInputArguments
    -- -- contents <- BL.readFile csvFile
    -- readResult <- try (BL.readFile csvFile):: IO(Either SomeException ByteString)
    -- case readResult of
    --      Left someException -> print someException
    --      Right contents -> do
    --      	let result = parserCSVFile contents options
    --      	print result
    --      	-- putStrLn result
    -- System.IO.putStrLn "fine"

    System.IO.putStrLn "Thanks"





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


