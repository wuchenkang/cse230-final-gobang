module AI where

import Data.List ( maximumBy )
import Game

putAI :: Game -> Game 
putAI game = do
    let scores = calculateAIScore game
    let (_, xs) = maximumBy (\x y -> compare (fst x) (fst y)) (zip scores [0..])
    let (x, y) = getIJfromIndex xs
    placePiece game x y


getIJfromIndex :: Int -> (Int, Int)
getIJfromIndex k = (i, j)
    where 
        i = k `div` 8
        j = k `mod` 8


calculateAIScore :: Game -> [Int]
calculateAIScore game = [calculateAIScoreAt game row col | row <- [0..8], col <- [0..8]]

calculateAIScoreAt :: Game -> Int -> Int -> Int
calculateAIScoreAt game i j = do
    let item = board game !! i !! j
    let row = getRowAt game i
    let col = getColumnAt game j
    let diagnoalLeft = getLeftDiagonalAt game (i - j)
    let diagnoalRight = getRightDiagonalAt game (i + j)
    let diagnoalLeftIndex = min i j
    let tempMin = min 8 (i + j)
    let diagnoalRightIndex = tempMin - i - 1
    let score1 = calculateAIScoreAtList j row
    let score2 = calculateAIScoreAtList i col
    let score3 = calculateAIScoreAtList diagnoalLeftIndex diagnoalLeft
    let score4 = calculateAIScoreAtList diagnoalRightIndex diagnoalRight
    if item == Empty then 0 else score1 + score2 + score3 + score4


calculateAIScoreAtList :: Int -> [Cell] -> Int
calculateAIScoreAtList k list = do
    let leftList = reverse (take k list)
    let rightList = drop (k+1) list
    let leftConNum = calculateContinueNum leftList
    let rightConNum = calculateContinueNum rightList
    let leftFirst = getFirstEle leftList
    let rightFirst = getFirstEle rightList
    ((10 ^ leftConNum) * leftFirst) + ((10 ^ rightConNum) * rightFirst)

getFirstEle :: [Cell] -> Int
getFirstEle [] = 0
getFirstEle [x]
    | x == Occ 0 = 2
    | x == Occ 1 = 1
    | otherwise = 0
getFirstEle (x:_) = if x == Occ 0 then 2 else 1

calculateContinueNum :: [Cell] -> Int
calculateContinueNum  [] = 0
calculateContinueNum  [_] = 1
calculateContinueNum  [x1, x2] = if x1 == x2 then 2 else 1
calculateContinueNum  x@(x1:x2:_)  = if x1 == x2 then 1 + calculateContinueNum (tail x) else 1

test1 :: [Cell]
test1 = [Occ 0, Occ 1, Occ 1, Occ 1, Empty, Occ 1, Occ 1, Occ 0]

test2 :: [Cell]
test2 = [Occ 1, Occ 0, Occ 1, Occ 0, Empty, Occ 1, Occ 0, Occ 0]

test3 :: [Cell]
test3 = [Occ 1, Occ 0, Empty, Occ 0, Empty, Occ 1, Occ 0, Occ 0]
