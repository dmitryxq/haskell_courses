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
       Option ['i'] ["input"] (ReqArg (\arg opt -> return opt {input = arg}) "STRING") "Input file",
       Option ['d'] ["delimeter"] (ReqArg (\arg opt -> return opt {splitterColumn = arg})"STRING") "Splitter Column",
       Option ['z'] ["percent"] (ReqArg (\arg opt -> return opt {percent = read arg :: Int}) "Int") "Percent training",
       Option ['y'] ["retryCount"] (ReqArg (\arg opt -> return opt {retryCount = read arg :: Int}) "Int") "retry Count",
       Option ['o'] ["output"] (ReqArg (\arg opt -> return opt {output = read arg :: PrintSource}) "STRING") "output file"


   ]

parseInputArguments :: [String] -> IO ParserCSVOption
parseInputArguments arguments = do
    let (actions, _, _) = getOpt RequireOrder specificOption arguments
    foldl (>>=) (return defaultCSVOption) actions
