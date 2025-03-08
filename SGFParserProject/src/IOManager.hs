-- src/IOManager.hs
module IOManager (readSGFFile, writeJSONFile) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding (encodeUtf8)
import Converters.SGFToJSON (convertToJSON)
import Control.Exception (try, IOException, throw)
import System.IO (hPutStrLn, stderr)

-- | Reads a file containing SGF text
readSGFFile :: FilePath -> IO String
readSGFFile path = do
  putStrLn $ "Reading SGF file from: " ++ path
  content <- TIO.readFile path
  putStrLn $ "File read successfully, " ++ show (T.length content) ++ " characters"
  return (T.unpack content)

-- | Writes the parsed SGF data (converted to JSON) into a file
writeJSONFile :: FilePath -> [(String, String)] -> IO ()
writeJSONFile path sgfData = do
  putStrLn $ "Converting " ++ show (length sgfData) ++ " SGF properties to JSON..."
  let jsonData = convertToJSON(sgfData)
  putStrLn $ "JSON generated, size: " ++ show (BL.length jsonData) ++ " bytes"
  putStrLn $ "Writing to file: " ++ path
  BL.writeFile path jsonData
  putStrLn "File written successfully"