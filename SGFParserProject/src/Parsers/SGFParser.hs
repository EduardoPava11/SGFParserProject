{-# LANGUAGE OverloadedStrings #-}
module Parsers.SGFParser (parseSGFWithTree) where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as T
import Control.Monad (void)
import Debug.Trace (trace)

type Parser = Parsec Void T.Text

----------------------------------------------------------------------
-- Data Types
----------------------------------------------------------------------

data SGFKeyValue = SGFKeyValue
  { key :: String
  , val :: String
  }
  deriving (Show, Eq)

data SGFNode = SGFNode [SGFKeyValue]
  deriving (Show, Eq)

data SGFGameTree = SGFGameTree [SGFNode] [SGFGameTree]
  deriving (Show, Eq)

----------------------------------------------------------------------
-- Whitespace Skipper
----------------------------------------------------------------------

sc :: Parser ()
sc = skipMany (choice [
    void spaceChar,
    void newline,
    void (try (string "//" >> manyTill anySingle (void newline <|> eof)))
  ])

----------------------------------------------------------------------
-- Parse a Single Property (like B[pd], SZ[19], etc.)
----------------------------------------------------------------------

sgfProperty :: Parser SGFKeyValue
sgfProperty = do
  -- parse property key (allow both upper and lowercase)
  propKey <- some letterChar
  -- parse bracketed values
  vals    <- some parseBracketValue
  return $ SGFKeyValue propKey (concat vals)

parseBracketValue :: Parser String
parseBracketValue = do
  _    <- char '['
  body <- many (noneOf [']'])
  _    <- char ']'
  return body

----------------------------------------------------------------------
-- Parse a Node (; ...)
----------------------------------------------------------------------

sgfNode :: Parser SGFNode
sgfNode = do
  _ <- char ';'  -- each node starts with a semicolon
  sc
  -- parse zero or more properties until we see ';','(', or ')' 
  props <- many (try (notFollowedBy (oneOf [';', '(', ')']) >> sgfProperty <* sc))
  return (SGFNode props)

----------------------------------------------------------------------
-- Parse a Game Tree (( ... ))
----------------------------------------------------------------------

sgfGameTree :: Parser SGFGameTree
sgfGameTree = do
  _ <- char '('
  sc
  -- one or more nodes
  nodes      <- some sgfNode
  -- zero or more subtrees (variations)
  variations <- many (try sgfGameTree)
  sc
  _ <- char ')'
  return (SGFGameTree nodes variations)

----------------------------------------------------------------------
-- Extract Main Variation
----------------------------------------------------------------------

extractMainVariation :: SGFGameTree -> [(String, String)]
extractMainVariation (SGFGameTree nodes subtrees) =
  let nodeProps = concatMap (\(SGFNode kvs) -> [(k,v) | SGFKeyValue k v <- kvs]) nodes
      restProps = case subtrees of
                    (next:_) -> extractMainVariation next  -- follow the first branch
                    []       -> []
  in nodeProps ++ restProps

----------------------------------------------------------------------
-- parseSGFWithTree
----------------------------------------------------------------------

parseSGFWithTree :: String -> Either String [(String, String)]
parseSGFWithTree input =
  let txt    = T.pack input
      result = runParser sgfGameTree "SGF" txt
  in case result of
       Left err -> Left (errorBundlePretty err)
       Right tree -> Right (extractMainVariation tree)