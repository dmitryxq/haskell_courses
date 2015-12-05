import Prelude
import Data.Conduit
import System.IO
import System.IO.Error
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Control.Monad

readFilePro :: FilePath -> Source (ResourceT IO) String
readFilePro file = bracketP
    (do h <- openFile file ReadMode
        putStrLn $ "{" ++ file ++ " open}"
        return h )
    (\h -> do
        hClose h
        putStrLn $ "{" ++ file ++ " closed}" )
    fromHandle
  where
    fromHandle h = forever $ liftIO (hGetLine h) >>= yield

main :: IO ()
main = runResourceT $ producer $$ CL.mapM_ (liftIO . putStrLn)

producer = readFilePro csvFile