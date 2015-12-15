module ParserService where 
import Types
import System.IO
import Data.Conduit
import Control.Monad.IO.Class
import Data.List.Split
import qualified Data.Conduit.List as CL

source :: String -> Source IO String
source filepath = do
    handle <- liftIO $ openFile filepath ReadMode
    loop handle
  where
    loop handle = do
        eof <- liftIO $ hIsEOF handle
        if eof
            then return ()
            else do
                c <- liftIO $ hGetLine handle
                yield c
                loop handle

fileSink :: Handle -> Sink String IO ()
fileSink handle = CL.mapM_ (hPutStrLn handle)

conduit :: (String -> [String]) -> ([Double] -> [Double]) -> Conduit String IO DataVector
conduit elementSplit lTransform = do
    str <- await

    case str of 
            Just s -> do
                let (res, className) = splitInClassNameAndVector . emptyFilter . elementSplit $ s
                if className /= "" then yield (lTransform res, className) else return ()
                conduit elementSplit lTransform
            _ -> return ()  


splitInClassNameAndVector :: [String] -> DataVector
splitInClassNameAndVector [] = ([], "")
splitInClassNameAndVector v = (vToDouble $ init v, last v)

vToDouble = map toDouble
    where toDouble = \x -> read x :: Double
emptyFilter = filter (/= "")



mapWithIndex :: ((Int , a) -> b) -> [a] -> [b]
mapWithIndex f xs = map f (zip [0..] xs)





elementSplitter :: String -> String -> [String]
elementSplitter devider = splitOneOf devider

deleteFirstColumn :: Bool -> [a] -> [a]
deleteFirstColumn option contents =   
    if option
        then tail contents
    else contents

deleteLastColumn :: Bool -> [a] -> [a]
deleteLastColumn option contents = 
    if option
        then init contents
    else contents 

deleteColumns :: [a] -> Bool -> Bool -> [a]
deleteColumns list fColumn lColumn = deleteLastColumn lColumn (deleteFirstColumn fColumn list) 

deleteHeader :: [a] -> Bool -> [a]
deleteHeader list option = 
    if option
        then tail list
    else list

normalizationPercent :: Int -> Int
normalizationPercent percent = max (min percent 100) 10