import Data.List (elemIndices, sort)
import Data.Set (fromList, toList)
import System.Console.Haskeline (InputT, runInputT, defaultSettings, getInputLine, outputStr, outputStrLn)
import System.Random (randomRIO)
import Text.Read (readMaybe)

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

-- TYPE DECLARATIONS.

type Board = [[Piece]]

type Position = (Int, Int)

data Piece = B | B' | O | O' | X | X' | AO | AO' | AX | AX'
             deriving (Eq, Show)

data Move = PushO | PushX | PushA | Push'
            deriving (Eq, Show)

data Tree a = Node a [Tree a]
              deriving Show

-- BOARD PRESETS.

blank :: Board
blank = replicate rows $ replicate cols B

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
                  where (countO, countX) = (count b [O, O'], count b [X, X'])

initiateAscend :: Board -> [Position] -> Board
initiateAscend b [] = b
initiateAscend b (p : ps) = initiateAscend ((\(Just b) -> b) (move b PushA p)) ps

settleAscend :: (Board, Int, Int) -> (Board, Int, Int)
settleAscend (b, hO, hX) | dO > dX = (b', hO, hX - if dO - dX > countAO then countAO + (dO - dX - countAO + 1) `div` 2 else dO - dX)
                         | otherwise = (b', hO - if dX - dO > countAX then countAX + (dX - dO - countAX + 1) `div` 2 else dX - dO, hX)
                           where b' = map (map (\p -> if elem p [AO, AO', AX, AX'] then B else p)) b
                                 (dO, dX) = (countAO + 2 * countAO', countAX + 2 * countAX')
                                 (countAO, countAO', countAX, countAX') = (count b [AO], count b [AO'], count b [AX], count b [AX'])

rowAt :: Board -> Position -> [(Piece, Position)]
rowAt b (r, _) = [(b !! r !! c, (r, c)) | c <- [0 .. cols - 1]]

colAt :: Board -> Position -> [(Piece, Position)]
colAt b (_, c) = [(b !! r !! c, (r, c)) | r <- [0 .. rows - 1]]

diag1At :: Board -> Position -> [(Piece, Position)]
diag1At b (r, c) = [(b !! r' !! c', (r', c')) | (r', c') <- zip [max 0 (r - c) .. min (rows - 1) (r - c + cols - 1)] [max 0 (c - r) .. min (cols - 1) (c - r + rows - 1)]]

diag2At :: Board -> Position -> [(Piece, Position)]
diag2At b (r, c) = [(b !! r' !! c', (r', c')) | (r', c') <- zip [max 0 (r + c - cols + 1) .. min (rows - 1) (r + c)] (reverse [max 0 (c + r - rows + 1) .. min (cols - 1) (r + c)])]

checkConnectRow :: [(Piece, Position)] -> (Piece, Position) -> [Position]
checkConnectRow pps pp = map snd (takeWhile (\(p, _) -> p == fst pp') (reverse pps1)) ++ map snd (takeWhile (\(p, _) -> p == fst pp') pps2)
                         where (pps1, pps2) = splitAt (head (elemIndices pp' pps')) pps'
                               (pps', pp') = ([(movePieceUnchecked p Push', pos) | (p, pos) <- pps], (movePieceUnchecked (fst pp) Push', snd pp))

checkConnect :: Board -> Position -> [Position]
checkConnect b (r, c) = toList $ fromList $ concat $ [checkConnectRow row (b !! r !! c, (r, c)) | row <- [rowAt b (r, c), colAt b (r, c), diag1At b (r, c), diag2At b (r, c)]]

checkAscendRow :: [(Piece, Position)] -> (Piece, Position) -> [Position]
checkAscendRow pps pp | length ps < ascend = []
                      | otherwise = ps
                        where ps = checkConnectRow pps pp

checkAscend :: Board -> Position -> [Position]
checkAscend b (r, c) = toList $ fromList $ concat $ [checkAscendRow row (b !! r !! c, (r, c)) | row <- [rowAt b (r, c), colAt b (r, c), diag1At b (r, c), diag2At b (r, c)]]

checkStepAscend :: Board -> Position -> [(Position, [Position])]
checkStepAscend b (r, c) = [(p, aps) | (b', p) <- zip bs ps, aps <- [checkAscend b' p], elem (r, c) aps]
                           where bs = map (\(Just b) -> b) [move b (if elem (b !! r !! c) [O, O'] then PushO else PushX) p | p <- ps]
                                 ps = map snd $ filter (\(p, _) -> elem p [B, B', AO, AO', AX, AX']) $ rowAt b (r, c) ++ colAt b (r, c) ++ diag1At b (r, c) ++ diag2At b (r, c)

checkOnlyAscend :: Board -> Move -> [Position]
checkOnlyAscend b m = findOnly $ filter (\(_, ps) -> ps /= []) [let b' = (\(Just b) -> b) (move b m (r, c)) in if elem (b !! r !! c) [B, B', AO, AO', AX, AX'] then ((r, c), filter (\p -> p /= (r, c)) (checkAscend b' (r, c))) else ((r, c), []) | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
                      where findOnly [] = []
                            findOnly (x : xs) = if elem (snd x) (map snd xs) then findOnly (filter (\(_, ps) -> ps /= snd x) xs) else (fst x) : findOnly xs

importance :: Board -> [Position] -> Int
importance _ [] = 0
importance b ((r, c) : ps) | elem (b !! r !! c) [B', O', X', AO', AX'] = 2 + importance b ps
                           | otherwise = 1 + importance b ps

density :: Board -> Move -> Int
density b PushO = sum [importance b (checkConnect b (r, c)) | r <- [0 .. rows - 1], c <- [0 .. cols - 1], elem (b !! r !! c) [O, O']] + sum [let b' = (\(Just b) -> b) (move b PushO (r, c)) in (length (checkConnect b' (r, c)) - 1) `div` 2 | r <- [0 .. rows - 1], c <- [0 .. cols - 1], elem (b !! r !! c) [B, B', AO, AO', AX, AX']]
density b PushX = sum [importance b (checkConnect b (r, c)) | r <- [0 .. rows - 1], c <- [0 .. cols - 1], elem (b !! r !! c) [X, X']] + sum [let b' = (\(Just b) -> b) (move b PushX (r, c)) in (length (checkConnect b' (r, c)) - 1) `div` 2 | r <- [0 .. rows - 1], c <- [0 .. cols - 1], elem (b !! r !! c) [B, B', AO, AO', AX, AX']]

counterMove :: Board -> Move -> Board
counterMove b m = initiateAscend b' (checkAscend b' p)
                  where b' = (\(Just b) -> b) (move b m p)
                        p = snd $ head $ reverse $ sort $ zip step2 [(r, c) | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
                        step2 = if all (== 0) step1 then [let b' = (\(Just b) -> b) (move b m (r, c)) in if elem (b !! r !! c) [AO, AO', AX, AX'] then importance b' (checkConnect b' (r, c)) else 0 | r <- [0 .. rows - 1], c <- [0 .. cols - 1]] else step1
                        step1 = [let b' = (\(Just b) -> b) (move b m (r, c)) in if elem (b !! r !! c) [B, B', AO, AO', AX, AX'] then importance b' (checkAscend b' (r, c)) else 0 | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]

healthBonus :: Move -> (Int, Int) -> (Int, Int) -> Int
healthBonus m (hO, hX) (hO', hX') | hO' <= 0 = if m == PushO then -99 else 999
                                  | hX' <= 0 = if m == PushO then 999 else -99
                                  | otherwise = 20 * if m == PushO then hO' - hO else hX' - hX

ascendBonus :: (Board, Int, Int) -> Move -> Bool -> [Int]
ascendBonus (b, hO, hX) m f | ascending b = [let b' = (\(Just b) -> b) (move b m (r, c)); b'' = initiateAscend b' (checkAscend b' (r, c)); d1 = density b'' m; d2 = density b'' (nextMove m) in if elem (b !! r !! c) [B, B', AO, AO', AX, AX'] && checkAscend b' (r, c) /= [] then 99 + d1 - d2 else 0 | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
                            | otherwise = [let b' = (\(Just b) -> b) (move b m (r, c)); ps = checkAscend b' (r, c); (b'', hO', hX') = settleAscend (counterMove (initiateAscend b' ps) (nextMove m), hO, hX); d1 = density b'' m; d2 = density b'' (nextMove m) in if elem (b !! r !! c) [B, B'] && ps /= [] then (if f && d1 <= ascend then -99 else 4) + (if f then d1 - d2 else 0) + healthBonus m (hO, hX) (hO', hX') else 0 | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]

heuristic :: (Board, Int, Int) -> Move -> Bool -> [(Int, Position)]
heuristic (b, hO, hX) m f = reverse $ sort $ zip step9 [(r, c) | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
                            where step9 = [if elem (b !! r !! c) [B, B', AO, AO', AX, AX'] then step8 !! (r * cols + c) else -999 | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
                                  step8 = [step7 !! (r * cols + c) + max (rows `div` 2) (cols `div` 2) - max (abs (rows `div` 2 - r)) (abs (cols `div` 2 - c)) | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
                                  step7 = [step6 !! (r * cols + c) + let b' = (\(Just b) -> b) (move b m (r, c)); d1 = density b m; d2 = density b' m in if elem (b !! r !! c) [B', AO', AX'] then (if d2 - d1 - importance b' [(r, c)] > 0 then 6 else 2) else 0 | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
                                  step6 = [step5 !! (r * cols + c) + if elem (b !! r !! c) [AO', AX'] then 4 else if elem (b !! r !! c) [AO, AX] then 2 else 0 | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
                                  step5 = map (\(x, y) -> x + y) $ zip step4 (ascendBonus (b, hO, hX) m f)
                                  step4 = [step3 !! (r * cols + c) + let b' = (\(Just b) -> b) (move b m (r, c)) in if elem (b !! r !! c) [B, B', AO, AO', AX, AX'] then (\n -> if n > importance b' [(r, c)] then n + 2 else n) (importance b' (checkConnect b' (r, c)) + importance b' (checkAscend b' (r, c))) else 0 | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
                                  step3 = [step2 !! (r * cols + c) + let b' = map (map (\p -> if elem p (if m == PushO then [X, X'] else [O, O']) then B else p)) ((\(Just b) -> b) (move b m (r, c))) in if elem (b !! r !! c) [B, B', AO, AO', AX, AX'] && checkAscend b' (r, c) == [] then max (sum [step2 !! (r' * cols + c') | (r', c') <- map fst (checkStepAscend b' (r, c))] - 4) 0 else 0 | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
                                  step2 = [step1 !! (r * cols + c) + if elem (b !! r !! c) (if m == PushO then [X, X'] else [O, O']) then importance b (checkConnect b (r, c)) + sum (map (importance b) (map snd (checkStepAscend b (r, c)))) else 0 | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
                                  step1 = let ps = checkOnlyAscend b (nextMove m) in [let b' = (\(Just b) -> b) (move b (nextMove m) (r, c)) in if elem (b !! r !! c) [B, B', AO, AO', AX, AX'] then importance b' (checkConnect b' (r, c)) + (if elem (r, c) ps then 2 else 1) * importance b' (checkAscend b' (r, c)) else 0 | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]

expand :: ((Int, Position), (Board, Int, Int)) -> Move -> Bool -> [((Int, Position), (Board, Int, Int))]
expand (_, (b, hO, hX)) m f | win (b, hO, hX) /= "" = []
                            | ascending b = [((h, p), settleAscend (b', hO, hX)) | ((h, p), b') <- hpbs]
                            | otherwise = [((h, p), (b', hO, hX)) | ((h, p), b') <- hpbs]
                              where hpbs = map (\((h, p), Just b') -> ((h, p), initiateAscend b' (checkAscend b' p))) $ take breadth $ filter (\(_, mb) -> mb /= Nothing) [((h, p), move b m p) | (h, p) <- heuristic (b, hO, hX) m f]

grow :: Tree ((Int, Position), (Board, Int, Int)) -> Move -> Bool -> Int -> Tree ((Int, Position), (Board, Int, Int))
grow t _ _ 0 = t
grow t m f n = grow (grow1 t) (nextMove m) (not f) (n - 1)
               where grow1 (Node x []) = Node x [Node x' [] | x' <- expand x m f]
                     grow1 (Node x ts) = Node x [grow1 t | t <- ts]

label :: Tree ((Int, Position), (Board, Int, Int)) -> Move -> Tree ((Int, Int, Position), (Board, Int, Int))
label (Node ((h, p), (b, hO, hX)) []) _ = Node ((hO - hX, h, p), (b, hO, hX)) []
label (Node ((h, p), x) ts) m = Node (((if m == PushO then maximum else minimum) ls, h, p), x) lts
                                where ls = map (\(Node ((l, _, _), _) _) -> l) lts
                                      lts = [label t (nextMove m) | t <- ts]

options :: (Board, Int, Int) -> Move -> [(Position, (Board, Int, Int))]
options x m = map (\((h, p), x') -> (p, x')) $ filter (\((h, _), _) -> h == maximum (map (\((h, _), _) -> h) hpxs)) hpxs
              where hpxs = map (\((l, h, p), x') -> ((h, p), x')) $ filter (\((l, _, _), _) -> l == (if m == PushO then maximum else minimum) (map (\((l, _, _), _) -> l) lhpxs)) lhpxs
                    lhpxs = map (\(Node lhpx _) -> lhpx) $ (\(Node _ ts) -> ts) lt
                    lt = label (grow (Node ((-1, (-1, -1)), x) []) m True depth) m

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

printBoard :: (Board, Int, Int) -> InputT IO ()
printBoard (b, hO, hX) = do
  outputStr $ unlines $ map concat $ map (map showPiece) b
  outputStrLn $ "Health O: " ++ show hO ++ " | Health X: " ++ show hX

inputHealth :: String -> InputT IO (Int)
inputHealth s = do
  ms <- getInputLine s
  if ms == Nothing then do
    outputStrLn "Invalid health."
    inputHealth s
  else do
    let mi = readMaybe ((\(Just s) -> s) ms) :: Maybe Int
    if mi == Nothing then do
      outputStrLn "Invalid health."
      inputHealth s
    else do
      return $ (\(Just i) -> i) mi

-- GAME.

main :: IO ()
main = runInputT defaultSettings start

start :: InputT IO ()
start = do
  ms <- getInputLine "Play as O or X? > "
  if ms == Nothing then do
    outputStrLn "Invalid option."
    start
  else do
    let s = (\(Just s) -> s) ms
    if s == "O" || s == "o" then do
      healthO <- inputHealth "Health O > "
      healthX <- inputHealth "Health X > "
      play (board, healthO, healthX) PushO True
    else if s == "X" || s == "x" then do
      healthO <- inputHealth "Health O > "
      healthX <- inputHealth "Health X > "
      play (board, healthO, healthX) PushO False
    else do
      outputStrLn "Invalid option."
      start

play :: (Board, Int, Int) -> Move -> Bool -> InputT IO ()
play (b, hO, hX) m isHuman = do
  printBoard (b, hO, hX)
  let s = win (b, hO, hX)
  if s /= "" then do
    outputStrLn s
  else do
    (b1, hO1, hX1) <- hmove (b, hO, hX) Push'
    printBoard (b1, hO1, hX1)
    if isHuman then do
      (b2, hO2, hX2) <- hmove (b1, hO1, hX1) m
      play (b2, hO2, hX2) (nextMove m) (not isHuman)
    else do
      (b2, hO2, hX2) <- cmove (b1, hO1, hX1) m
      play (b2, hO2, hX2) (nextMove m) (not isHuman)

hmove :: (Board, Int, Int) -> Move -> InputT IO (Board, Int, Int)
hmove (b, hO, hX) Push' = do
  ms <- getInputLine "Grass positions :: [(Int, Int)] > "
  if ms == Nothing then do
    outputStrLn "Invalid potisions."
    hmove (b, hO, hX) Push'
  else do
    let s = (\(Just s) -> s) ms
    let mps = readMaybe s :: Maybe [Position]
    if mps == Nothing then do
      outputStrLn "Invalid potisions."
      hmove (b, hO, hX) Push'
    else do
      let ps = map (\(x, y) -> (x - 1, y - 1)) $ (\(Just ps) -> ps) mps
      let mb1 = growGrass b ps
      if mb1 == Nothing then do
        outputStrLn "Invalid potisions."
        hmove (b, hO, hX) Push'
      else do
        return ((\(Just b) -> b) mb1, hO, hX)
hmove (b, hO, hX) m = do
  ms <- getInputLine $ show m !! 4 : " position :: (Int, Int) > "
  if ms == Nothing then do
    outputStrLn "Invalid potision."
    hmove (b, hO, hX) m
  else do
    let s = (\(Just s) -> s) ms
    let mp = readMaybe s :: Maybe Position
    if mp == Nothing then do
      outputStrLn "Invalid potision."
      hmove (b, hO, hX) m
    else do
      let p = (\(x, y) -> (x - 1, y - 1)) $ (\(Just p) -> p) mp
      let mb1 = move b m p
      if mb1 == Nothing then do
        outputStrLn "Invalid potision."
        hmove (b, hO, hX) m
      else do
        let b1 = (\(Just b) -> b) mb1
        let b2 = initiateAscend b1 (checkAscend b1 p)
        if ascending b then do
          return $ settleAscend (b2, hO, hX)
        else do
          return (b2, hO, hX)

cmove :: (Board, Int, Int) -> Move -> InputT IO (Board, Int, Int)
cmove (b, hO, hX) m = do
  outputStrLn $ show m !! 4 : " is thinking..."
  let opts = options (b, hO, hX) m
  i <- randomRIO (0, length opts - 1)
  let ((r, c), (b', hO', hX')) = opts !! i
  outputStrLn $ show m !! 4 : " moved to " ++ show (r + 1, c + 1) ++ "."
  return (b', hO', hX')
