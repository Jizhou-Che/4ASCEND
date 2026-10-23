module Game.IO where

import Data.Char (toUpper)
import System.Console.Haskeline (InputT, getInputLine, outputStr, outputStrLn)
import Text.Read (readMaybe)

import Game.Types

printPiece :: Piece -> String
printPiece B = "  "
printPiece B' = " '"
printPiece O = "◦ "
printPiece O' = "◦'"
printPiece X = "• "
printPiece X' = "•'"
printPiece AO = "☆ "
printPiece AO' = "☆'"
printPiece AX = "★ "
printPiece AX' = "★'"

printBoard :: (Board, Int, Int) -> InputT IO ()
printBoard (b, hO, hX) = do
  outputStr $ unlines $ map concat $ map (map printPiece) b
  outputStrLn $ "Health O: " ++ show hO ++ " | Health X: " ++ show hX

inputPlayer :: String -> InputT IO Bool
inputPlayer str = do
  ms <- getInputLine str
  case ms of
    Nothing -> do
      outputStrLn "Invalid option."
      inputPlayer str
    Just s -> case map toUpper s of
      "O" -> return True
      "X" -> return False
      _ -> do
        outputStrLn "Invalid option."
        inputPlayer str

inputHealth :: String -> InputT IO Int
inputHealth str = do
  ms <- getInputLine str
  case ms of
    Nothing -> do
      outputStrLn "Invalid health."
      inputHealth str
    Just s -> do
      let mi = readMaybe s :: Maybe Int
      case mi of
        Nothing -> do
          outputStrLn "Invalid health."
          inputHealth str
        Just i -> return i

inputGrass :: String -> InputT IO [Position]
inputGrass str = do
  ms <- getInputLine str
  case ms of
    Nothing -> do
      outputStrLn "Invalid grass positions."
      inputGrass str
    Just s -> do
      let mps = readMaybe s :: Maybe [Position]
      case mps of
        Nothing -> do
          outputStrLn "Invalid grass positions."
          inputGrass str
        Just ps -> return $ map (\(r, c) -> (r - 1, c - 1)) ps

inputPiece :: String -> InputT IO (Int, Int)
inputPiece str = do
  ms <- getInputLine str
  case ms of
    Nothing -> do
      outputStrLn "Invalid position."
      inputPiece str
    Just s -> do
      let mp = readMaybe s :: Maybe Position
      case mp of
        Nothing -> do
          outputStrLn "Invalid position."
          inputPiece str
        Just p -> return $ (\(r, c) -> (r - 1, c - 1)) p
