module Main where

import BayesClassifier
import ParserService
import ParserArgumentParams
import MathService
import Types

import System.Environment
import System.IO
import Data.Conduit
import System.Random
import Control.Monad.State
import Options.Applicative
import Data.List
import Text.Printf
import qualified Data.Conduit.List as CL
import qualified Data.Map as M
import Control.Parallel.Strategies as Strategies
import Control.Monad.Par as Par

naiveBayes :: ParserCSVOption -> IO ()
naiveBayes options = do
    let ParserCSVOption {splitterColumn = devider} = options
        ParserCSVOption {input = filePath} = options
        ParserCSVOption {percent = percent} = options
        ParserCSVOption {ignoreFirstColumn = skipFirstCol} = options
        ParserCSVOption {ignoreLastColumn = skipLastCol} = options
        ParserCSVOption {ignoreHeader = skipFirstLine} = options
        ParserCSVOption {retryCount = retryCount} = options
        ParserCSVOption {output = output} = options

    let result :: IO [DataVector]
        source' = source filePath
        lTransform list = deleteColumns list skipLastCol skipFirstCol 
        conduit' = conduit (elementSplitter devider) lTransform
        result = source' $$ conduit' =$ CL.consume
    v <- result
    let vectors = deleteHeader v skipFirstLine
    gen <- getStdGen

    let res = bestTrainedData vectors retryCount (normalizationPercent percent) gen    
    let resultSource = CL.sourceList $ bayesResultSource res

    case output of
        Just outFile -> do 
            h <- openFile outFile WriteMode
            resultSource $$ (fileSink h)
            hClose h
        Nothing -> resultSource $$ awaitForever (lift . print)
    


printAttribute :: (Int, (Double, Double)) -> String
printAttribute (index, (mu, disp)) = printf " - %d(%.2f;%.2f)" index mu disp

printTrainedClass :: (String , M.Map Int (Double, Double)) -> String
printTrainedClass (className, attrMap) = className ++ arrtName
                                            where attrNames = runPar $ Par.parMap printAttribute $ sortOn fst (M.toList attrMap)
                                                  arrtName = concat attrNames

bayesResultSource :: TrainedData -> [String]
bayesResultSource trained = runPar $ Par.parMap printTrainedClass trainedList
                                where trainedClassF attrData = M.map meanAndDispersian attrData
                                      trainedList = M.toList . M.map trainedClassF $ trained


main :: IO ()
main = do
    options <- getArgs >>= parseInputArguments
    naiveBayes options
    putStrLn "Finish"


