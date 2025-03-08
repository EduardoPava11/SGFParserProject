-- src/UI.hs
module UI (runCLI) where

import IOManager (readSGFFile, writeJSONFile)
import Parsers.SGFParser (parseSGFWithTree)
import System.Directory (createDirectoryIfMissing)
import System.IO (hPutStrLn, stderr)
import Control.Exception (try, IOException)

runCLI :: IO ()
runCLI = do
  putStrLn "===== SGF to JSON Parser ====="
  putStrLn "Enter the path to the SGF file:"
  filePath <- getLine
  
  -- Handle file reading errors
  fileResult <- try (readSGFFile filePath) :: IO (Either IOException String)
  case fileResult of
    Left err -> do
      hPutStrLn stderr $ "Error reading file: " ++ show err
      return ()
    Right sgfContent -> do
      -- Only call parseSGFWithTree now:
      case parseSGFWithTree sgfContent of
        Left parseErr -> do
          putStrLn "Parsing SGF failed:"
          putStrLn parseErr
          putStrLn $ "First 100 chars: " ++ take 100 sgfContent
        Right props -> do
          putStrLn ("Successfully parsed SGF with " ++ show (length props) ++ " properties")
          -- ensure output dir
          createDirectoryIfMissing True "output"
          putStrLn "Processing moves..."
          
          -- Handle JSON conversion errors
          jsonResult <- try (writeJSONFile "output/game.json" props) :: IO (Either IOException ())
          case jsonResult of
            Left err -> putStrLn $ "Error writing JSON: " ++ show err
            Right _  -> putStrLn "Parsed SGF and saved to output/game.json!"