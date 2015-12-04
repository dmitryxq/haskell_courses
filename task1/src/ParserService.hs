module ParserService
    ( 
        parserCSVFile
    ) where

import Types
import Data.List.Split
import Data.Csv
import Data.Char
import Data.ByteString.Lazy
import Data.Vector as V
import qualified Data.ByteString.Lazy as BL

type VectorMatrix a = V.Vector a
type Matrix a = V.Vector (VectorMatrix a)

-- public
parserCSVFile :: ByteString -> ParserCSVOption -> IO(Either String (Matrix String))
parserCSVFile contents options = 
    let ParserCSVOption {splitterColumn = splitter} = options
        ParserCSVOption {ignoreHeader = ignoreHeader} = options
        decodeOptions = DecodeOptions { decDelimiter = fromIntegral (ord splitter) }

        -- delete header if needed
        resultData = deleteHeader contents decodeOptions ignoreHeader 

    in decodingData resultData options

-- private
decodingData :: Either String (Matrix String) -> ParserCSVOption -> IO(Either String (Matrix String)) 
decodingData infoData options = case infoData of 
    Left errorMessage -> return (Left errorMessage)
    Right parsedData -> return (Right (V.map (V.map read) (deleteLastColumn options (deleteFirstColumn options parsedData))))


-- private
deleteHeader :: ByteString -> DecodeOptions -> Bool -> Either String (Matrix String)
deleteHeader contents decodeOptions ignoreHeader = 
    if ignoreHeader
        then decodeWith decodeOptions HasHeader contents
    else decodeWith decodeOptions NoHeader contents

-- private
deleteFirstColumn :: ParserCSVOption -> (Matrix String) -> (Matrix String)
deleteFirstColumn options contents = 
    let ParserCSVOption {ignoreFirstColumn = ignoreColumn} = options
    in    
    if ignoreColumn
        then V.map V.init contents
    else contents 

-- private
deleteLastColumn :: ParserCSVOption -> (Matrix String) -> (Matrix String)
deleteLastColumn options contents = 
    let ParserCSVOption {ignoreLastColumn = ignoreColumn} = options
    in    
    if ignoreColumn
        then V.map V.init contents
    else contents 
