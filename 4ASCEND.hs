import Data.List (elemIndices, sort)
import Data.Set (fromList, toList)

import System.IO
import System.Random

----------------------------------------------------------------

-- CONSTANTS.

ascend :: Int
ascend = 4

rows :: Int
rows = 9

cols :: Int
cols = 9

board :: Board
board = blank

breadth :: Int
breadth = 9

depth :: Int
depth = 4

----------------------------------------------------------------

-- TYPE DECLARATIONS.

type Board = [[Piece]]

type Position = (Int, Int)

data Piece = B | B' | O | O' | X | X' | AO | AO' | AX | AX'
             deriving (Eq, Show)

data Move = PushO | PushX | PushA | Push'
            deriving (Eq, Show)

data Tree a = Node a [Tree a]
              deriving Show

----------------------------------------------------------------

-- BOARD PRESETS.

blank :: Board
blank = replicate rows $ replicate cols B

----------------------------------------------------------------

-- CORE UTILITIES.

nextMove :: Move -> Move
nextMove PushO = PushX
nextMove PushX = PushO

movePiece :: Piece -> Move -> Maybe Piece
movePiece B PushO = Just O
movePiece B' PushO = Just O'
movePiece AO PushO = Just O
movePiece AO' PushO = Just O'
movePiece AX PushO = Just O
movePiece AX' PushO = Just O'
movePiece _ PushO = Nothing
movePiece B PushX = Just X
movePiece B' PushX = Just X'
movePiece AO PushX = Just X
movePiece AO' PushX = Just X'
movePiece AX PushX = Just X
movePiece AX' PushX = Just X'
movePiece _ PushX = Nothing
movePiece O PushA = Just AO
movePiece O' PushA = Just AO'
movePiece X PushA = Just AX
movePiece X' PushA = Just AX'
movePiece _ PushA = Nothing
movePiece B Push' = Just B'
movePiece O Push' = Just O'
movePiece X Push' = Just X'
movePiece AO Push' = Just AO'
movePiece AX Push' = Just AX'
movePiece _ Push' = Nothing

movePieceUnchecked :: Piece -> Move -> Piece
movePieceUnchecked p m | mp == Nothing = p
                       | otherwise = (\(Just p) -> p) mp
                         where mp = movePiece p m

setPiece :: Board -> Piece -> Position -> Board
setPiece b p (r, c) = take r b ++ [row'] ++ drop (r + 1) b
                      where row' = take c row ++ [p] ++ drop (c + 1) row
                            row = b !! r

move :: Board -> Move -> Position -> Maybe Board
move b m (r, c) | r < 0 || r >= rows || c < 0 || c >= cols || mp == Nothing = Nothing
                | otherwise = Just $ setPiece b ((\(Just p) -> p) mp) (r, c)
                  where mp = movePiece (b !! r !! c) m

growGrass :: Board -> [Position] -> Maybe Board
growGrass b [] = Just b
growGrass b ((r, c) : ps) | r < 0 || r >= rows || c < 0 || c >= cols || mb == Nothing = Nothing
                          | otherwise = growGrass ((\(Just b) -> b) mb) ps
                            where mb = move b Push' (r, c)

full :: Board -> Bool
full b = all (== True) $ map (all (\p -> elem p [O, O', X, X'])) b

ascending :: Board -> Bool
ascending b = any (== True) $ map (any (\p -> elem p [AO, AO', AX, AX'])) b

count :: Board -> [Piece] -> Int
count b ps = length $ concat $ map (filter (\p -> elem p ps)) b

win :: (Board, Int, Int) -> String
win (b, hO, hX) | hO <= 0 = "X wins!"
                | hX <= 0 = "O wins!"
                | full b = if hO > hX then "O wins!" else if hX > hO then "X wins!" else if countO > countX then "O wins!" else if countO < countX then "X wins!" else "Draw!"
                | otherwise = ""
                  where countO = count b [O, O']
                        countX = count b [X, X']

initiateAscend :: Board -> [Position] -> Board
initiateAscend b [] = b
initiateAscend b (p : ps) = initiateAscend ((\(Just b) -> b) (move b PushA p)) ps

clearAscend :: Board -> Board
clearAscend b = map (\row -> [clearAscendPiece p | p <- row]) b
                where clearAscendPiece p | elem p [AO, AO', AX, AX'] = B
                                         | otherwise = p

settleAscend :: (Board, Int, Int) -> (Board, Int, Int)
settleAscend (b, hO, hX) | dO > dX = (clearAscend b, hO, hX - dO + dX)
                         | dO < dX = (clearAscend b, hO + dO - dX, hX)
                         | otherwise = (clearAscend b, hO, hX)
                           where dO = count b [AO] + 2 * count b [AO']
                                 dX = count b [AX] + 2 * count b [AX']

rowPiece :: Board -> Position -> [(Piece, Position)]
rowPiece b (r, _) = [(b !! r !! c, (r, c)) | c <- [0 .. cols - 1]]

colPiece :: Board -> Position -> [(Piece, Position)]
colPiece b (_, c) = [(b !! r !! c, (r, c)) | r <- [0 .. rows - 1]]

diag1Piece :: Board -> Position -> [(Piece, Position)]
diag1Piece b (r, c) = [(b !! r' !! c', (r', c')) | (r', c') <- zip [max 0 (r - c) .. min (rows - 1) (r - c + cols - 1)] [max 0 (c - r) .. min (cols - 1) (c - r + rows - 1)]]

diag2Piece :: Board -> Position -> [(Piece, Position)]
diag2Piece b (r, c) = [(b !! r' !! c', (r', c')) | (r', c') <- zip [max 0 (r + c - cols + 1) .. min (rows - 1) (r + c)] (reverse [max 0 (c + r - rows + 1) .. min (cols - 1) (r + c)])]

checkConnectRow :: [(Piece, Position)] -> (Piece, Position) -> [Position]
checkConnectRow pps pp = ps1 ++ ps2
                         where ps1 = map snd $ takeWhile (\(p, _) -> p == fst pp') (reverse pps1)
                               ps2 = map snd $ takeWhile (\(p, _) -> p == fst pp') pps2
                               (pps1, pps2) = splitAt (head (elemIndices pp' pps')) pps'
                               pp' = (movePieceUnchecked (fst pp) Push', snd pp)
                               pps' = [(movePieceUnchecked p Push', pos) | (p, pos) <- pps]

checkConnect :: Board -> Position -> [Position]
checkConnect b (r, c) = toList $ fromList $ concat $ [checkConnectRow row (b !! r !! c, (r, c)) | row <- [rowPiece b (r, c), colPiece b (r, c), diag1Piece b (r, c), diag2Piece b (r, c)]]

checkAscendRow :: [(Piece, Position)] -> (Piece, Position) -> [Position]
checkAscendRow pps pp | length ps < ascend = []
                      | otherwise = ps
                        where ps = checkConnectRow pps pp

checkAscend :: Board -> Position -> [Position]
checkAscend b (r, c) = toList $ fromList $ concat $ [checkAscendRow row (b !! r !! c, (r, c)) | row <- [rowPiece b (r, c), colPiece b (r, c), diag1Piece b (r, c), diag2Piece b (r, c)]]

checkStepAscend :: Board -> Position -> [(Int, Position)]
checkStepAscend b (r, c) = filter (\(n, _) -> n /= 0) [(if elem (r, c) aps then length aps else 0, p) | (b', p) <- zip bs ps, aps <- [checkAscend b' p]]
                           where bs = map (\(Just b) -> b) [move b (if elem (b !! r !! c) [O, O'] then PushO else PushX) p | p <- ps]
                                 ps = map snd $ filter (\(p, _) -> elem p [B, B', AO, AO', AX, AX']) $ rowPiece b (r, c) ++ colPiece b (r, c) ++ diag1Piece b (r, c) ++ diag2Piece b (r, c)

heuristic :: Board -> Move -> [(Int, Position)]
heuristic b m = reverse $ sort $ zip step8 [(r, c) | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
                where step8 = [if elem (b !! r !! c) [B, B', AO, AO', AX, AX'] then step7 !! (r * cols + c) else -1 | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
                      step7 = [step6 !! (r * cols + c) + max (rows `div` 2) (cols `div` 2) - max (abs (rows `div` 2 - r)) (abs (cols `div` 2 - c)) | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
                      step6 = [step5 !! (r * cols + c) + if elem (b !! r !! c) [AO, AO', AX, AX'] then 10 else 0 | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
                      step5 = [step4 !! (r * cols + c) + if elem (b !! r !! c) [B, B', AO, AO', AX, AX'] && checkAscend b' (r, c) == [] then sum [step4 !! (r' * cols + c') | (r', c') <- map snd (checkStepAscend b' (r, c))] else 0 | r <- [0 .. rows - 1], c <- [0 .. cols - 1], b' <- [map (map (\p -> if elem p (if m == PushO then [X, X'] else [O, O']) then B else p)) ((\(Just b) -> b) (move b m (r, c)))]]
                      step4 = [step3 !! (r * cols + c) + if elem (b !! r !! c) [B, B', AO, AO', AX, AX'] then length (checkConnect b' (r, c)) + length (checkAscend b' (r, c)) else 0 | r <- [0 .. rows - 1], c <- [0 .. cols - 1], b' <- [(\(Just b) -> b) (move b m (r, c))]]
                      step3 = [step2 !! (r * cols + c) + if elem (b !! r !! c) (if m == PushO then [X, X'] else [O, O']) then length (checkConnect b (r, c)) + sum (map fst (checkStepAscend b (r, c))) else 0 | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
                      step2 = [step1 !! (r * cols + c) + if elem (b !! r !! c) [B, B', AO, AO', AX, AX'] then length (checkConnect b' (r, c)) + length (checkAscend b' (r, c)) else 0 | r <- [0 .. rows - 1], c <- [0 .. cols - 1], b' <- [(\(Just b) -> b) (move b (nextMove m) (r, c))]]
                      step1 = [if elem (b !! r !! c) [B', O', X', AO', AX'] then 2 else 0 | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]

expand :: ((Int, Position), (Board, Int, Int)) -> Move -> [((Int, Position), (Board, Int, Int))]
expand (_, (b, hO, hX)) m | win (b, hO, hX) /= "" = []
                          | ascending b = [((h, p), settleAscend (b', hO, hX)) | ((h, p), b') <- hpbs]
                          | otherwise = [((h, p), (b', hO, hX)) | ((h, p), b') <- hpbs]
                            where hpbs = map (\((h, p), Just b') -> ((h, p), initiateAscend b' (checkAscend b' p))) $ take breadth $ filter (\(_, mb) -> mb /= Nothing) [((h, p), move b m p) | (h, p) <- heuristic b m]

grow :: Tree ((Int, Position), (Board, Int, Int)) -> Move -> Tree ((Int, Position), (Board, Int, Int))
grow (Node x []) m = Node x [Node x' [] | x' <- expand x m]
grow (Node x ts) m = Node x [grow t m | t <- ts]

growN :: Tree ((Int, Position), (Board, Int, Int)) -> Move -> Int -> Tree ((Int, Position), (Board, Int, Int))
growN t _ 0 = t
growN t m n = growN (grow t m) (nextMove m) (n - 1)

label :: Tree ((Int, Position), (Board, Int, Int)) -> Move -> Tree ((Int, Int, Position), (Board, Int, Int))
label (Node ((h, p), (b, hO, hX)) []) _ = Node ((hO - hX, h, p), (b, hO, hX)) []
label (Node ((h, p), x) ts) m = Node (((if m == PushO then maximum else minimum) ls, h, p), x) lts
                                where ls = map (\(Node ((l, _, _), _) _) -> l) lts
                                      lts = [label t (nextMove m) | t <- ts]

options :: (Board, Int, Int) -> Move -> [(Position, (Board, Int, Int))]
options x m = map (\((h, p), x') -> (p, x')) $ filter (\((h, _), _) -> h == maximum (map (\((h, _), _) -> h) hpxs)) hpxs
              where hpxs = map (\((l, h, p), x') -> ((h, p), x')) $ filter (\((l, _, _), _) -> l == (if m == PushO then maximum else minimum) (map (\((l, _, _), _) -> l) lhpxs)) lhpxs
                    lhpxs = map (\(Node lhpx _) -> lhpx) $ (\(Node _ ts) -> ts) lt
                    lt = label (growN (Node ((-1, (-1, -1)), x) []) m depth) m

----------------------------------------------------------------

-- IO UTILITIES.

showPiece :: Piece -> String
showPiece B = "  "
showPiece B' = " '"
showPiece O = "◦ "
showPiece O' = "◦'"
showPiece X = "• "
showPiece X' = "•'"
showPiece AO = "☆ "
showPiece AO' = "☆'"
showPiece AX = "★ "
showPiece AX' = "★'"

printBoard :: (Board, Int, Int) -> IO ()
printBoard (b, hO, hX) = do
  putStr $ unlines $ map concat $ map (map showPiece) b
  putStrLn $ "Health O: " ++ show hO ++ " | Health X: " ++ show hX

inputHealth :: IO (Int, Int)
inputHealth = do
  putStr "Health O > "
  hFlush stdout
  healthO <- getLine
  putStr "Health X > "
  hFlush stdout
  healthX <- getLine
  return (read healthO :: Int, read healthX :: Int)

----------------------------------------------------------------

-- GAME.

main :: IO ()
main = do
  putStr "Play as O or X? > "
  hFlush stdout
  c <- getLine
  if c == "O" || c == "o" then do
    (healthO, healthX) <- inputHealth
    play (board, healthO, healthX) PushO True
  else if c == "X" || c == "x" then do
    (healthO, healthX) <- inputHealth
    play (board, healthO, healthX) PushO False
  else do
    putStrLn "Invalid option."
    main

play :: (Board, Int, Int) -> Move -> Bool -> IO ()
play (b, hO, hX) m isHuman = do
  printBoard (b, hO, hX)
  let s = win (b, hO, hX)
  if s /= "" then do
    putStrLn s
  else do
    (b1, hO1, hX1) <- hmove (b, hO, hX) Push'
    printBoard (b1, hO1, hX1)
    if isHuman then do
      (b2, hO2, hX2) <- hmove (b1, hO1, hX1) m
      play (b2, hO2, hX2) (nextMove m) (not isHuman)
    else do
      (b2, hO2, hX2) <- cmove (b1, hO1, hX1) m
      play (b2, hO2, hX2) (nextMove m) (not isHuman)

hmove :: (Board, Int, Int) -> Move -> IO (Board, Int, Int)
hmove (b, hO, hX) Push' = do
  putStr "Grass positions :: [(Int, Int)] > "
  hFlush stdout
  input <- getLine
  let ps = map (\(x, y) -> (x - 1, y - 1)) $ read input :: [Position]
  let mb1 = growGrass b ps
  if mb1 == Nothing then do
    putStrLn "Invalid potisions."
    hmove (b, hO, hX) Push'
  else do
    return ((\(Just b) -> b) mb1, hO, hX)
hmove (b, hO, hX) m = do
  putStr $ show m !! 4 : " position :: (Int, Int) > "
  hFlush stdout
  input <- getLine
  let p = (\(x, y) -> (x - 1, y - 1)) $ read input :: Position
  let mb1 = move b m p
  if mb1 == Nothing then do
    putStrLn "Invalid potision."
    hmove (b, hO, hX) m
  else do
    let b1 = (\(Just b) -> b) mb1
    let b2 = initiateAscend b1 (checkAscend b1 p)
    if ascending b then do
      return $ settleAscend (b2, hO, hX)
    else do
      return (b2, hO, hX)

cmove :: (Board, Int, Int) -> Move -> IO (Board, Int, Int)
cmove (b, hO, hX) m = do
  putStrLn $ show m !! 4 : " is thinking..."
  let opts = options (b, hO, hX) m
  i <- randomRIO (0, length opts - 1)
  let ((r, c), (b', hO', hX')) = opts !! i
  putStrLn $ show m !! 4 : " moved to " ++ show (r + 1, c + 1) ++ "."
  return (b', hO', hX')
