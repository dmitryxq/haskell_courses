module ParserArgumentParams
    (
      parseInputArguments,
      specificOption
    ) where

import System.Console.GetOpt
import Types

specificOption :: [ OptDescr (ParserCSVOption -> IO ParserCSVOption)]
specificOption =
   [
       Option ['h'] ["header"] (NoArg (\opt -> return opt {ignoreHeader = True})) "Ignore header",
       Option ['f'] ["first"] (NoArg (\opt -> return opt {ignoreFirstColumn = True})) "Ignore first column",
       Option ['l'] ["last"] (NoArg (\opt -> return opt {ignoreLastColumn = True})) "Ignore last column",
       Option ['e'] ["euclid"] (NoArg (\opt -> return opt {typeDistance = True})) "Euclid distance",
       Option ['m'] ["hamming"] (NoArg (\opt -> return opt {typeDistance = False})) "Hamming distance",
       Option ['p'] ["eps"] (ReqArg (\arg opt -> return opt {epsilon = read arg :: Double}) "DOUBLE") "EPS",
       Option ['h'] ["help"] (NoArg (\opt -> return opt {help = True})) "Help",
       Option ['i'] ["input"] (ReqArg (\arg opt -> return opt {input = arg}) "STRING") "Input file",
       Option ['d'] ["delimeter"] (ReqArg (\arg opt -> return opt {splitterColumn = arg !! 0})"Char") "Splitter Column"
   ]

parseInputArguments :: [String] -> IO ParserCSVOption
parseInputArguments arguments = do
    let (actions, _, _) = getOpt RequireOrder specificOption arguments
    foldl (>>=) (return defaultCSVOption) actions
