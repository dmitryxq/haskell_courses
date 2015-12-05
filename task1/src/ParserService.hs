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
type CustomMatrix a = V.Vector (VectorMatrix a)

-- public
parserCSVFile :: ByteString -> ParserCSVOption -> IO(Either String (CustomMatrix String))
parserCSVFile contents options = 
    let ParserCSVOption {splitterColumn = splitter} = options
        ParserCSVOption {ignoreHeader = ignoreHeader} = options
        decodeOptions = DecodeOptions { decDelimiter = fromIntegral (ord splitter) }

        -- delete header if needed
        resultData = deleteHeader contents decodeOptions ignoreHeader 

    in decodingData resultData options

-- private
decodingData :: Either String (CustomMatrix String) -> ParserCSVOption -> IO(Either String (CustomMatrix String)) 
decodingData infoData options = case infoData of 
    Left errorMessage -> return (Left errorMessage)
    Right parsedData -> return (Right (V.map (V.map read) (deleteLastColumn options (deleteFirstColumn options parsedData))))


-- private
deleteHeader :: ByteString -> DecodeOptions -> Bool -> Either String (CustomMatrix String)
deleteHeader contents decodeOptions ignoreHeader = 
    if ignoreHeader
        then decodeWith decodeOptions HasHeader contents
    else decodeWith decodeOptions NoHeader contents

-- private
deleteFirstColumn :: ParserCSVOption -> (CustomMatrix String) -> (CustomMatrix String)
deleteFirstColumn options contents = 
    let ParserCSVOption {ignoreFirstColumn = ignoreColumn} = options
    in    
    if ignoreColumn
        then V.map V.init contents
    else contents 

-- private
deleteLastColumn :: ParserCSVOption -> (CustomMatrix String) -> (CustomMatrix String)
deleteLastColumn options contents = 
    let ParserCSVOption {ignoreLastColumn = ignoreColumn} = options
    in    
    if ignoreColumn
        then V.map V.init contents
    else contents 
