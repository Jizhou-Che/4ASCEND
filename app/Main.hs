import System.Console.Haskeline (InputT, defaultSettings, outputStrLn, runInputT)
import System.Random (randomRIO)

import Game.Boards
import Game.Core
import Game.IO
import Game.Types

main :: IO ()
main = runInputT defaultSettings start

start :: InputT IO ()
start = do
  humanFirst <- inputPlayer "Play as O or X? > "
  healthO <- inputHealth "Health O > "
  healthX <- inputHealth "Health X > "
  play (board, healthO, healthX) TurnO humanFirst

play :: (Board, Int, Int) -> Turn -> Bool -> InputT IO ()
play (b, hO, hX) t isHuman = do
  printBoard (b, hO, hX)
  let s = win (b, hO, hX)
  case s of
    "" -> do
      (b1, hO1, hX1) <- gmove (b, hO, hX)
      printBoard (b1, hO1, hX1)
      (b2, hO2, hX2) <- (if isHuman then hmove else cmove) (b1, hO1, hX1) t
      play (b2, hO2, hX2) (nextTurn t) (not isHuman)
    _ -> outputStrLn s

gmove :: (Board, Int, Int) -> InputT IO (Board, Int, Int)
gmove (b, hO, hX) = do
  ps <- inputGrass "Grass positions :: [(Int, Int)] > "
  let mb1 = growGrass b ps
  case mb1 of
    Nothing -> do
      outputStrLn "Invalid grass positions."
      gmove (b, hO, hX)
    Just b1 -> return (b1, hO, hX)

hmove :: (Board, Int, Int) -> Turn -> InputT IO (Board, Int, Int)
hmove (b, hO, hX) t = do
  p <- inputPiece $ show t ++ " position :: (Int, Int) > "
  let mb1 = move b (if t == TurnO then PushO else PushX) p
  case mb1 of
    Nothing -> do
      outputStrLn "Invalid position."
      hmove (b, hO, hX) t
    Just b1 -> return $ (if ascending b then settleAscend else id) (initiateAscend b1 p, hO, hX)

cmove :: (Board, Int, Int) -> Turn -> InputT IO (Board, Int, Int)
cmove (b, hO, hX) t = do
  outputStrLn $ show t ++ " is thinking..."
  let opts = options (b, hO, hX) t
  i <- randomRIO (0, length opts - 1)
  let ((r, c), (b', hO', hX')) = opts !! i
  outputStrLn $ show t ++ " moved to " ++ show (r + 1, c + 1) ++ "."
  return (b', hO', hX')
