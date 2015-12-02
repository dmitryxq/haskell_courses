module ParserService
    ( 
    ) where

import Types
import Data.List.Split
import Data.Csv
import Data.Char
import Data.ByteString.Lazy
import Data.Vector as V
-- import Data.Text (Text)

import qualified Data.ByteString.Lazy as BL

-- parserCSVFile :: FilePath -> ParserCSVOption -> IO CSV
-- parserCSVFile filePath options = do
--     fileData <- BL.readFile filePath
--     let ParserCSVOption {splitterColumn = delimetr} = options
--         resultData 
--     in case result of
--                         Left err -> Left err
--                         Right parsedData -> Right    


parserCSVFile :: ByteString -> ParserCSVOption -> Either String (Vector (Vector Double))
parserCSVFile contents options = 
    let ParserCSVOption {input = file} = options
        ParserCSVOption {splitterColumn = splitter} = options
        decodeOptions = DecodeOptions { decDelimiter = fromIntegral (ord ',')}
        
        resultData = if ignoreHeader options
                        then decodeWith decodeOptions HasHeader contents
                     else decodeWith decodeOptions NoHeader contents
        in case resultData of Left errorMessage -> Left errorMessage
                              Right parsedData -> Right (V.map (V.map read) parsedData)









-- parser :: ParserCSVOption -> String -> [[String]]
-- parser opts s = 
--     let rows = if ignoreHeader opts then deleteNewLinesWithoutHeader s else deleteNewLines s
--         getRow = case (ignoreFirstColumn opts, ignoreLastColumn opts) of
--                      (True, True) -> init . tail . splitOn splitterColumn opts
--                      (True, False) -> tail . splitOn splitterColumn opts
--                      (False, True) -> init . splitOn splitterColumn opts
--                      (False, False) -> splitOn splitterColumn opts
--     in
-- 	[getRow line | line <- rows]

-- parseCsv :: FilePath -> ParserCSVOption -> IO [[String]]
-- parseCsv f opts = do
--     s <- readFile f
--     return (parser opts s)

-- deleteNewLines :: String -> [String]  
-- deleteNewLines str = lines str  

-- deleteNewLinesWithoutHeader :: String -> [String]  
-- deleteNewLinesWithoutHeader str = tail lines str  