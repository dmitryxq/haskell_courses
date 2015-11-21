module Main where

import MathService
import System.IO
import System.IO.Error
import Data.Conduit
import Data.Conduit.Binary
import Data.CSV.Conduit
import Data.Text (Text)
import qualified Data.ByteString as B

-- | File for tested
csvFile :: FilePath
csvFile = "resources/glass.txt"

main :: IO ()
main = putStrLn $ show $ hammingDistance [1, 2, 3] [2, 3, 1, 5]
-- main = do
-- 	putStrLn "Please, write input.csv file:"
-- 	csvFile <- getLine
-- 	putStrLn "Count clasters:"
-- 	countClaster <- getLine
-- 	putStrLn "Write epsilon:"
-- 	epsilon <- getLine
-- 	putStrLn "Thanks"
