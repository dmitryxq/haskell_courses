module Main where

import Data.Conduit
import Data.Conduit.Binary
import Data.CSV.Conduit
import Data.Text (Text)
import qualified Data.ByteString as byteString

-- | File where the CSV is stored.
csvFile :: FilePath
csvFile = "resources/glass.txt"

main :: IO ()
main = do
	file <- byteString.readFile csvFile
	putStrLn "reading file"
