-- test/Spec.hs
module Main where

import Test.HUnit
import Parsers.SGFParser (parseSGFWithTree)

-- Single node with properties
testParseSGFMinimal :: Test
testParseSGFMinimal = TestCase $ do
  let sgf = "(;GM[1]SZ[19]PB[BlackPlayer]PW[WhitePlayer])"
  case parseSGFWithTree sgf of
    Left err -> assertFailure ("Parsing minimal SGF failed: " ++ err)
    Right result -> do
      let expected =
            [ ("GM","1")
            , ("SZ","19")
            , ("PB","BlackPlayer")
            , ("PW","WhitePlayer")
            ]
      assertEqual "Should parse single-node SGF" expected result

-- Multi-node example
testParseSGFMultiNode :: Test
testParseSGFMultiNode = TestCase $ do
  let sgf = "(;GM[1]SZ[19]PB[BlackPlayer]PW[WhitePlayer];B[pd];W[dd])"
  case parseSGFWithTree sgf of
    Left err -> assertFailure ("Parsing multi-node SGF failed: " ++ err)
    Right result -> do
      let expected =
            [ ("GM","1")
            , ("SZ","19")
            , ("PB","BlackPlayer")
            , ("PW","WhitePlayer")
            , ("B","pd")
            , ("W","dd")
            ]
      assertEqual "Should parse multi-node SGF" expected result

main :: IO ()
main = do
  _ <- runTestTT (TestList [testParseSGFMinimal, testParseSGFMultiNode])
  return ()