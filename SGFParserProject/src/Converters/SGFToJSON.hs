-- src/Converters/SGFToJSON.hs
{-# LANGUAGE OverloadedStrings #-}
module Converters.SGFToJSON (convertToJSON) where

import Data.Aeson (encode, object, (.=), Value)  -- Added Value here
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Game.Engine (initialGameState, applyMove)
import Game.Types (Move(..), Stone(..), GameState(..))

convertToJSON :: [(String, String)] -> BL.ByteString
convertToJSON sgfData =
  let size       = read (maybe "19" id (lookup "SZ" sgfData)) :: Int
      blackName  = maybe "Unknown" id (lookup "PB" sgfData)
      whiteName  = maybe "Unknown" id (lookup "PW" sgfData)

      -- main line moves (B[...] or W[...])
      moves = filter (\(k,_) -> k=="B" || k=="W") sgfData

      initGS = initialGameState size
      allStates = buildStates initGS moves

      framesJson = zipWith makeFrame [1..] allStates
        where
          makeFrame :: Int -> GameState -> Value
          makeFrame n st = object [ "moveNumber"    .= n
                                  , "board"         .= boardTo2D st
                                  , "capturedBlack" .= capturedBlack st
                                  , "capturedWhite" .= capturedWhite st
                                  ]
  in encode $
       object
         [ "gameInfo" .= object
             [ "boardSize"   .= size
             , "blackPlayer" .= blackName
             , "whitePlayer" .= whiteName
             ]
         , "frames" .= framesJson
         ]

buildStates :: GameState -> [(String, String)] -> [GameState]
buildStates gs [] = []
buildStates gs ((color, coords):rest) =
  let move = case coords of
               "" -> Pass  -- empty => pass
               _  -> let (r,c) = sgfCoordsToRC coords
                     in Place r c

      nextGS =
        case color of
          "B" | currentPlayer gs == Black -> applyOrStay move gs
              | otherwise                 -> gs -- or handle error
          "W" | currentPlayer gs == White -> applyOrStay move gs
              | otherwise                 -> gs
          _                               -> gs
  in nextGS : buildStates nextGS rest

applyOrStay :: Move -> GameState -> GameState
applyOrStay mv st =
  case applyMove mv st of
    Left _err   -> st  -- ignore errors
    Right stNew -> stNew

boardTo2D :: GameState -> [[String]]
boardTo2D st =
  let b   = board st
      sz  = boardSize st
  in [ [ stoneToString (b V.! (r*sz + c)) | c <- [0..sz-1] ]
       | r <- [0..sz-1]
     ]

stoneToString :: Stone -> String
stoneToString Black = "B"
stoneToString White = "W"
stoneToString Empty = "E"

sgfCoordsToRC :: String -> (Int, Int)
sgfCoordsToRC [x,y] = (letterToNum x, letterToNum y)
sgfCoordsToRC _     = (0,0)

letterToNum :: Char -> Int
letterToNum c = fromEnum c - fromEnum 'a'