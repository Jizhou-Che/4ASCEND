module Game.Core where

import Data.List (sort)
import Data.Maybe (fromJust)
import Data.Set (fromList, toList)

import Game.Constants
import Game.Types

nextTurn :: Turn -> Turn
nextTurn TurnO = TurnX
nextTurn TurnX = TurnO

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
movePieceUnchecked p m = case movePiece p m of
  Nothing -> p
  Just p' -> p'

setPiece :: Board -> Piece -> Position -> Board
setPiece b p (r, c) = take r b ++ [row'] ++ drop (r + 1) b
  where
    row = b !! r
    row' = take c row ++ [p] ++ drop (c + 1) row

move :: Board -> Move -> Position -> Maybe Board
move b m (r, c)
  | r < 0 || r >= rows || c < 0 || c >= cols = Nothing
  | otherwise = (\p -> setPiece b p (r, c)) <$> movePiece (b !! r !! c) m

growGrass :: Board -> [Position] -> Maybe Board
growGrass b [] = Just b
growGrass b ((r, c) : ps) = move b Push' (r, c) >>= \b' -> growGrass b' ps

full :: Board -> Bool
full b = all (== True) $ map (all (\p -> elem p [O, O', X, X'])) b

ascending :: Board -> Bool
ascending b = any (== True) $ map (any (\p -> elem p [AO, AO', AX, AX'])) b

count :: Board -> [Piece] -> Int
count b ps = length $ concat $ map (filter (\p -> elem p ps)) b

win :: (Board, Int, Int) -> String
win (b, hO, hX)
  | hO <= 0 = "X wins!"
  | hX <= 0 = "O wins!"
  | full b = let countO = count b [O, O']; countX = count b [X, X'] in if hO > hX then "O wins!" else if hO < hX then "X wins!" else if countO > countX then "O wins!" else if countO < countX then "X wins!" else "Draw!"
  | otherwise = ""

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
  where
    (pps', pp') = ([(movePieceUnchecked p Push', pos) | (p, pos) <- pps], (movePieceUnchecked (fst pp) Push', snd pp))
    (pps1, pps2) = break (== pp') pps'

checkConnect :: Board -> Position -> [Position]
checkConnect b (r, c) = toList $ fromList $ concat $ [checkConnectRow row (b !! r !! c, (r, c)) | row <- [rowAt b (r, c), colAt b (r, c), diag1At b (r, c), diag2At b (r, c)]]

checkAscendRow :: [(Piece, Position)] -> (Piece, Position) -> [Position]
checkAscendRow pps pp = let ps = checkConnectRow pps pp in if length ps < ascend then [] else ps

checkAscend :: Board -> Position -> [Position]
checkAscend b (r, c) = toList $ fromList $ concat $ [checkAscendRow row (b !! r !! c, (r, c)) | row <- [rowAt b (r, c), colAt b (r, c), diag1At b (r, c), diag2At b (r, c)]]

checkStepAscend :: Board -> Position -> [(Position, [Position])]
checkStepAscend b (r, c) = [(p, aps) | (b', p) <- zip bs ps, aps <- [checkAscend b' p], elem (r, c) aps]
  where
    ps = map snd $ filter (\(p, _) -> elem p [B, B', AO, AO', AX, AX']) $ rowAt b (r, c) ++ colAt b (r, c) ++ diag1At b (r, c) ++ diag2At b (r, c)
    bs = map fromJust [move b (if elem (b !! r !! c) [O, O'] then PushO else PushX) p | p <- ps]

checkOnlyAscend :: Board -> Turn -> [Position]
checkOnlyAscend b t = findOnly $ filter (\(_, ps) -> ps /= []) [let b' = fromJust (move b (if t == TurnO then PushO else PushX) (r, c)) in if elem (b !! r !! c) [B, B', AO, AO', AX, AX'] then ((r, c), filter (\p -> p /= (r, c)) (checkAscend b' (r, c))) else ((r, c), []) | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
  where
    findOnly [] = []
    findOnly (x : xs) = if elem (snd x) (map snd xs) then findOnly (filter (\(_, ps) -> ps /= snd x) xs) else (fst x) : findOnly xs

initiateAscend :: Board -> Position -> Board
initiateAscend b p = foldl (\brd pos -> fromJust (move brd PushA pos)) b (checkAscend b p)

settleAscend :: (Board, Int, Int) -> (Board, Int, Int)
settleAscend (b, hO, hX)
  | dO > dX = (b', hO, hX - if dO - dX > countAO then countAO + (dO - dX - countAO + 1) `div` 2 else dO - dX)
  | otherwise = (b', hO - if dX - dO > countAX then countAX + (dX - dO - countAX + 1) `div` 2 else dX - dO, hX)
  where
    (countAO, countAO', countAX, countAX') = (count b [AO], count b [AO'], count b [AX], count b [AX'])
    (dO, dX) = (countAO + 2 * countAO', countAX + 2 * countAX')
    b' = map (map (\p -> if elem p [AO, AO', AX, AX'] then B else p)) b

importance :: Board -> [Position] -> Int
importance _ [] = 0
importance b ((r, c) : ps)
  | elem (b !! r !! c) [B', O', X', AO', AX'] = 2 + importance b ps
  | otherwise = 1 + importance b ps

density :: Board -> Turn -> Int
density b t = sum [importance b (checkConnect b (r, c)) | r <- [0 .. rows - 1], c <- [0 .. cols - 1], elem (b !! r !! c) (if t == TurnO then [O, O'] else [X, X'])] + sum [let b' = fromJust (move b (if t == TurnO then PushO else PushX) (r, c)) in (length (checkConnect b' (r, c)) - 1) `div` 2 | r <- [0 .. rows - 1], c <- [0 .. cols - 1], elem (b !! r !! c) [B, B', AO, AO', AX, AX']]

counterMove :: Board -> Turn -> Board
counterMove b t = initiateAscend b' p
  where
    step1 = [let brd = fromJust (move b (if t == TurnO then PushO else PushX) (r, c)) in if elem (b !! r !! c) [B, B', AO, AO', AX, AX'] then importance brd (checkAscend brd (r, c)) + (if elem (b !! r !! c) [AO, AO', AX, AX'] then importance b [(r, c)] else 0) else 0 | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
    step2 = if all (== 0) step1 then [let brd = fromJust (move b (if t == TurnO then PushO else PushX) (r, c)) in if elem (b !! r !! c) [AO, AO', AX, AX'] then importance brd (checkConnect brd (r, c)) else 0 | r <- [0 .. rows - 1], c <- [0 .. cols - 1]] else step1
    p = snd $ maximum $ zip step2 [(r, c) | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
    b' = fromJust (move b (if t == TurnO then PushO else PushX) p)

healthBonus :: Turn -> (Int, Int) -> (Int, Int) -> Int
healthBonus t (hO, hX) (hO', hX')
  | hO' <= 0 = if t == TurnO then -99 else 999
  | hX' <= 0 = if t == TurnO then 999 else -99
  | otherwise = 20 * if t == TurnO then hO' - hO else hX' - hX

ascendBonus :: (Board, Int, Int) -> Turn -> Bool -> [Int]
ascendBonus (b, hO, hX) t f
  | ascending b = [let b' = fromJust (move b (if t == TurnO then PushO else PushX) (r, c)); b'' = initiateAscend b' (r, c); d1 = density b'' t; d2 = density b'' (nextTurn t) in if elem (b !! r !! c) [B, B', AO, AO', AX, AX'] && checkAscend b' (r, c) /= [] then 99 + d1 - d2 else 0 | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
  | otherwise = [let b' = fromJust (move b (if t == TurnO then PushO else PushX) (r, c)); (b'', hO', hX') = settleAscend (counterMove (initiateAscend b' (r, c)) (nextTurn t), hO, hX); d1 = density b'' t; d2 = density b'' (nextTurn t) in if elem (b !! r !! c) [B, B'] && checkAscend b' (r, c) /= [] then (if f && (d1 <= 2 * ascend || count b'' (if t == TurnO then [O, O'] else [X, X']) <= ascend) then -99 else 6) + (if f then d1 - d2 else 0) + healthBonus t (hO, hX) (hO', hX') else 0 | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]

heuristic :: (Board, Int, Int) -> Turn -> Bool -> [(Int, Position)]
heuristic (b, hO, hX) t f = reverse $ sort $ zip step9 [(r, c) | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
  where
    step1 = let ps = checkOnlyAscend b (nextTurn t) in [let b' = fromJust (move b (if t == TurnO then PushX else PushO) (r, c)) in if elem (b !! r !! c) [B, B', AO, AO', AX, AX'] then importance b' (checkConnect b' (r, c)) + (if elem (r, c) ps then 2 else 1) * importance b' (checkAscend b' (r, c)) else 0 | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
    step2 = [step1 !! (r * cols + c) + if elem (b !! r !! c) (if t == TurnO then [X, X'] else [O, O']) then importance b (checkConnect b (r, c)) + sum (map (importance b) (map snd (checkStepAscend b (r, c)))) else 0 | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
    step3 = [step2 !! (r * cols + c) + let b' = map (map (\p -> if elem p (if t == TurnO then [X, X'] else [O, O']) then B else p)) (fromJust (move b (if t == TurnO then PushO else PushX) (r, c))) in if elem (b !! r !! c) [B, B', AO, AO', AX, AX'] && checkAscend b' (r, c) == [] then max (sum [step2 !! (r' * cols + c') | (r', c') <- map fst (checkStepAscend b' (r, c))] - 4) 0 else 0 | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
    step4 = [step3 !! (r * cols + c) + let b' = fromJust (move b (if t == TurnO then PushO else PushX) (r, c)) in if elem (b !! r !! c) [B, B', AO, AO', AX, AX'] then (\n -> if n > importance b' [(r, c)] then n + 2 else n) (importance b' (checkConnect b' (r, c)) + importance b' (checkAscend b' (r, c))) else 0 | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
    step5 = map (\(x, y) -> x + y) $ zip step4 (ascendBonus (b, hO, hX) t f)
    step6 = [step5 !! (r * cols + c) + if elem (b !! r !! c) [AO', AX'] then 4 else if elem (b !! r !! c) [AO, AX] then 2 else 0 | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
    step7 = [step6 !! (r * cols + c) + let b' = fromJust (move b (if t == TurnO then PushO else PushX) (r, c)) in if elem (b !! r !! c) [B', AO', AX'] then (if length (checkConnect b' (r, c)) == 1 then 2 else 6) else 0 | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
    step8 = [step7 !! (r * cols + c) + max (rows `div` 2) (cols `div` 2) - max (abs (rows `div` 2 - r)) (abs (cols `div` 2 - c)) | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
    step9 = [if elem (b !! r !! c) [B, B', AO, AO', AX, AX'] then step8 !! (r * cols + c) else -999 | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]

bestLabel :: [Label] -> Turn -> Label
bestLabel ls TurnO = let bestHealth = maximum (map fst ls); ls' = filter (\l -> fst l == bestHealth) ls in (bestHealth, minimum (map snd ls'))
bestLabel ls TurnX = let bestHealth = maximum (map snd ls); ls' = filter (\l -> snd l == bestHealth) ls in (minimum (map fst ls'), bestHealth)

minimax :: ((Int, Position), (Board, Int, Int)) -> Int -> Turn -> Bool -> (Label, Label) -> Tree ((Label, Int, Position), (Board, Int, Int))
minimax ((h, p), (b, hO, hX)) n t f (alpha, beta)
  | win (b, hO, hX) /= "" || n == 0 || (n == 1 && not (ascending b)) = Node (((hO, hX), h, p), (b, hO, hX)) []
  | otherwise = Node ((bestLabel (map (\(Node ((l, _, _), _) _) -> l) lts) t, h, p), (b, hO, hX)) lts
  where
    hpxs = map (\(h', p', mb') -> ((h', p'), (if ascending b then settleAscend else id) (initiateAscend (fromJust mb') p', hO, hX))) $ filter (\(_, _, mb) -> mb /= Nothing) $ take breadth [(h', p', move b (if t == TurnO then PushO else PushX) p') | (h', p') <- heuristic (b, hO, hX) t f]
    lts = alphabeta hpxs n t f (alpha, beta)

alphabeta :: [((Int, Position), (Board, Int, Int))] -> Int -> Turn -> Bool -> (Label, Label) -> [Tree ((Label, Int, Position), (Board, Int, Int))]
alphabeta [] _ _ _ _ = []
alphabeta (x : xs) n t f (alpha, beta)
  | bestLabel [alpha', beta'] TurnO == alpha' = [lt]
  | otherwise = lt : alphabeta xs n t f (alpha', beta')
  where
    lt = minimax x (n - 1) (nextTurn t) (not f) (alpha, beta)
    l = (\(Node ((lbl, _, _), _) _) -> lbl) lt
    (alpha', beta') = if t == TurnO then (bestLabel [alpha, l] t, beta) else (alpha, bestLabel [beta, l] t)

options :: (Board, Int, Int) -> Turn -> [(Position, (Board, Int, Int))]
options x t = map (\((_, p), x') -> (p, x')) $ filter (\((h, _), _) -> h == maximum (map (\((heu, _), _) -> heu) hpxs)) hpxs
  where
    lt = minimax ((0, (0, 0)), x) depth t True ((-999, 999), (999, -999))
    lhpxs = map (\(Node lhpx _) -> lhpx) $ (\(Node _ ts) -> ts) lt
    hpxs = map (\((_, h, p), x') -> ((h, p), x')) $ filter (\((l, _, _), _) -> l == bestLabel (map (\((lbl, _, _), _) -> lbl) lhpxs) t) lhpxs
