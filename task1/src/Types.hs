module Types
    (defaultCSVOption, 
    ParserCSVOption(..),
    Dictionary,
    TrainedData,
    DataVector,
    PrintSource
    ) where
import qualified Data.Map as M

data ParserCSVOption = ParserCSVOption
    {
        splitterColumn :: String
      , ignoreHeader :: Bool
      , ignoreFirstColumn :: Bool
      , ignoreLastColumn :: Bool 
      , input :: String
      , output :: PrintSource
      , percent :: Int
      , retryCount :: Int
    }
-- | File for tested
csvFile :: Maybe String
csvFile = Just "resources/output.txt"

defaultCSVOption :: ParserCSVOption
defaultCSVOption = ParserCSVOption
    { 
        splitterColumn = ","
      , ignoreHeader = False
      , ignoreFirstColumn = False
      , ignoreLastColumn = False
      , input = "resources/glass.txt"
      , output = csvFile
      , percent = 50
      , retryCount = 3
    }  

type Dictionary = M.Map Int [Double]
type TrainedData  = M.Map String Dictionary   

type DataVector   = ([Double], String)
type PrintSource = Maybe String