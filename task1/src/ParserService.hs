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



-- public
parserCSVFile :: ByteString -> ParserCSVOption -> Either String (Vector (Vector Double))
parserCSVFile contents options = 
    let ParserCSVOption {splitterColumn = splitter} = options
        ParserCSVOption {ignoreHeader = ignoreHeader} = options
        decodeOptions = DecodeOptions { decDelimiter = fromIntegral (ord splitter) }

        -- delete header if needed
        resultData = deleteHeader contents decodeOptions ignoreHeader 

    in decodingData resultData options

-- private
decodingData :: Either String (Vector (Vector String)) -> ParserCSVOption -> Either String (Vector (Vector Double))  
decodingData infoData options = case infoData of 
    Left errorMessage -> Left errorMessage
    Right parsedData -> Right (V.map (V.map read) (deleteLastColumn options (deleteFirstColumn options parsedData)))


-- private
deleteHeader :: ByteString -> DecodeOptions -> Bool -> Either String (Vector (Vector String))
deleteHeader contents decodeOptions ignoreHeader = 
    if ignoreHeader
        then decodeWith decodeOptions HasHeader contents
    else decodeWith decodeOptions NoHeader contents

-- private
deleteFirstColumn :: ParserCSVOption -> Vector(Vector String) -> Vector(Vector String)
deleteFirstColumn options contents = 
    let ParserCSVOption {ignoreFirstColumn = ignoreColumn} = options
    in    
    if ignoreColumn
        then V.map V.init contents
    else contents 

-- private
deleteLastColumn :: ParserCSVOption -> Vector(Vector String) -> Vector(Vector String)
deleteLastColumn options contents = 
    let ParserCSVOption {ignoreLastColumn = ignoreColumn} = options
    in    
    if ignoreColumn
        then V.map V.init contents
    else contents 
