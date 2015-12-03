module Types
    (defaultCSVOption, 
    ParserCSVOption(..)
    ) where

data ParserCSVOption = ParserCSVOption
    {
        splitterColumn :: Char
      , ignoreHeader :: Bool
      , ignoreFirstColumn :: Bool
      , ignoreLastColumn :: Bool
      , typeDistance :: Bool
      , epsilon :: Double
      , help :: Bool
      , input :: String
      , countCluster :: Integer
      , randomize :: Bool
    }


defaultCSVOption :: ParserCSVOption
defaultCSVOption = ParserCSVOption
    { 
        splitterColumn = ','
      , ignoreHeader = False
      , ignoreFirstColumn = False
      , ignoreLastColumn = False
      , typeDistance = False
      , epsilon = 0.01
      , help = False
      , input = "resources/glass.txt"
      , countCluster = 3
      , randomize = False
    }  