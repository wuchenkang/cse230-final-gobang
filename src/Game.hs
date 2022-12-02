module Game where

import Types
import Data.List (isInfixOf)
import Data.List.Split (chunksOf)
import Lens.Micro ( (&), (.~), ix )
import Brick ( modify, EventM, put, get )
import Brick.BChan ( writeBChan, BChan )
import Control.Concurrent ( threadDelay ) 
import Control.Concurrent.STM
    ( atomically, readTVarIO, writeTVar, TVar )
import Control.Monad.Trans (liftIO)
import Data.List ( maximumBy )
import Network.Socket ( Socket )
import NetUtil
import Control.Monad (when)


mkGame :: Setup -> [Int] -> Int -> TVar TimerStatus -> BChan GobangEvent -> Maybe Socket -> Game
mkGame su ib t s chan sock = Game 
  { 
    board = chunksOf 9 $ mkCell <$> ib
  , focusPos = (4, 4)
  , player = initiative su
  , identity = iden su
  , tictoc = t
  , timeLimit = t
  , timerStatus = s
  , mode = if typ su == 0 then Local else if typ su == 1 then AI else Online (iden su - 1)
  , status = Playing
  , gchan = chan
  , msock = sock
  , difficulty = if diff su == 1 then Easy else Hard
  }
  where
    mkCell 0 = Empty
    mkCell i = Occ i

moveCursor :: CursorDirection -> Game -> Game
moveCursor d g = 
  move d (focusPos g) 
  & (\p -> g { focusPos = p })
  where
    move :: CursorDirection -> (Int, Int) -> (Int, Int)
    move North (x, y) = (x, bdCheck $ y - 1)
    move South (x, y) = (x, bdCheck $ y + 1)
    move East  (x, y) = (bdCheck $ x + 1, y)
    move West  (x, y) = (bdCheck $ x - 1, y)
    bdCheck x | x < 0     = 0
            | x > 8     = 8
            | otherwise = x

isOccupied :: Game -> Int -> Int -> Bool
isOccupied game r c = helper $ board game !! r !! c
  where 
    helper Empty = False
    helper _     = True

placePiece :: Game -> Int -> Int -> Game
placePiece game row col = game { board = board game & ix row . ix col .~ p }
  where
    p = Occ $ player game 

placeFocus :: Game -> Game
placeFocus game = placePiece game y x
  where (x, y) = focusPos game

randomPlace :: Game -> Game
randomPlace game = game { board = board game & ix y . ix x .~ p }
  where
    (y, x) = head candidates
    candidates = [(row, col) | row <- [0 .. 8], 
                               col <- [0 .. 8],
                               not $ isOccupied game row col]
    p = Occ $ player game

timerUpdate :: Int -> Game -> Game
timerUpdate c game = game { tictoc = c }

turnOnTimer :: Game -> IO ()
turnOnTimer game = do
  atomically $ writeTVar (timerStatus game) ON

turnOffTimer :: Game -> IO ()
turnOffTimer game = do
  atomically $ writeTVar (timerStatus game) OFF

tictocThread :: TVar TimerStatus -> BChan GobangEvent -> IO ()
tictocThread status chan = do
  s <- readTVarIO status
  case s of
    ON -> do 
      threadDelay 1000000
      writeBChan chan Countdown
    _ -> do
      threadDelay 1000000

winGame :: Game -> Bool
winGame game = rowHit || colHit || diagHit
  where
    rowHit = any (listHit p) (getRows game)
    colHit = any (listHit p) (getColumns game)
    diagHit = any (listHit p) (getDiagonals game)
    p = player game

listHit :: Int -> [Cell] -> Bool
listHit _ [] = False
listHit p xs =  replicate 5 (Occ p) `isInfixOf` xs || listHit p (tail xs)

drawGame :: Game -> Bool
drawGame game = notElem Empty $ concat $ board game

detectState :: Game -> Game
detectState game  
  | winGame game = game {status = Win $ player game}
  | drawGame game = game {status = Draw}
  | otherwise = game {status = Playing}

switchPlayer :: Game -> Game
switchPlayer game | p == 1 = game { player = 2 }
                  | otherwise = game { player = 1 }
    where 
      p = player game

isYourTerm :: Game -> Bool
isYourTerm game = player game == identity game

afterPlacement :: EventM () Game ()
afterPlacement = do
  modify detectState
  game <- get
  case status game of 
    Win _ -> return ()
    Draw  -> return ()
    _     -> do
      -- reset and turn on/off timer
      let game' = timerUpdate (timeLimit game) game
      put game'
      -- change stage
      case mode game' of
        Local -> liftIO $ turnOnTimer game'
        AI -> do
          when (isYourTerm game') $ do 
              let (y, x) = putAI game'
              liftIO $ writeBChan (gchan game') (Placement (x, y))
              liftIO $ turnOnTimer game'
        Online _ -> 
          when (isYourTerm game') $ do
            let (c, r) = focusPos game'
            let ms = msock game'
            case ms of
              Nothing -> return ()
              (Just sock) -> liftIO $ sendPlacement c r sock
      modify switchPlayer
      return ()

getColumnAt :: [[Cell]] -> Int -> [Cell]
getColumnAt game j = [game !! row !! j | row <- [0..8]] 

getRowAt :: [[Cell]] -> Int -> [Cell]
getRowAt game i = game !! i

getLeftDiagonalAt :: [[Cell]] -> Int -> [Cell]
getLeftDiagonalAt game diff = [game !! row !! col | row <- [0..8], col <- [0..8], row - col == diff]

getRightDiagonalAt :: [[Cell]] -> Int -> [Cell]
getRightDiagonalAt game s = [game !! row !! col | row <- [0..8], col <- [0..8], row + col == s]

getRows :: Game -> [[Cell]]
getRows game = [getRowAt (board game) i | i <- [0..8]]

getColumns :: Game -> [[Cell]]
getColumns game = [getColumnAt (board game) i | i <- [0..8]]

getDiagonals :: Game -> [[Cell]]
getDiagonals game = [getLeftDiagonalAt (board game) i | i <- [-8..8]] ++ [getRightDiagonalAt (board game) i | i <- [0..16]]
-- AI Section
putAI :: Game -> (Int, Int)
putAI game = do
    let boardNow = board game
    let d = difficulty game
    let scores = calculateAIScore boardNow d
    let (_, xs) = maximumBy (\x y -> compare (fst x) (fst y)) (zip scores [0..])
    getIJfromIndex xs

putAITesting :: [[Cell]] -> Difficulty -> (Int, Int)
putAITesting boardNow d = do
    let scores = calculateAIScore boardNow d
    let (_, xs) = maximumBy (\x y -> compare (fst x) (fst y)) (zip scores [0..])
    getIJfromIndex xs


getIJfromIndex :: Int -> (Int, Int)
getIJfromIndex k = (i, j)
    where 
        i = k `div` 9
        j = k `mod` 9


calculateAIScore :: [[Cell]] -> Difficulty -> [Int]
calculateAIScore game d = 
  case d of 
    Easy -> [calculateAIScoreAt calculateAIScoreAtListEasy game row col | row <- [0..8], col <- [0..8]]
    Hard -> [calculateAIScoreAt calculateAIScoreAtListHard game row col | row <- [0..8], col <- [0..8]]

calculateAIScoreAt :: (Int -> [Cell] -> Int) -> [[Cell]] -> Int -> Int -> Int
calculateAIScoreAt func game i j = do
    let item = game !! i !! j
    let row = getRowAt game i
    let col = getColumnAt game j
    let diagnoalLeft = getLeftDiagonalAt game (i - j)
    let diagnoalRight = getRightDiagonalAt game (i + j)
    let diagnoalLeftIndex = min i j
    let tempMin = min 8 (i + j)
    let diagnoalRightIndex = tempMin - j
    let score1 = func j row
    let score2 = func i col
    let score3 = func diagnoalLeftIndex diagnoalLeft
    let score4 = func diagnoalRightIndex diagnoalRight
    let centerScore = (calculateCenterScore i j)
    if item == Empty then score1 + score2 + score3 + score4 + centerScore else centerScore

calculateCenterScore :: Int -> Int -> Int
calculateCenterScore i j = do
  let diffx = min i 8-i
  let diffy = min j 8-j
  diffx * diffy 
  
calculateAIScoreAtListEasy :: Int -> [Cell] -> Int
calculateAIScoreAtListEasy k list = do
    let leftList = reverse (take k list)
    let rightList = drop (k+1) list
    let leftConNum = calculateContinueNum leftList
    let rightConNum = calculateContinueNum rightList
    let leftFirst = getFirstEle leftList
    let rightFirst = getFirstEle rightList
    ((10 ^ leftConNum) * leftFirst) + ((10 ^ rightConNum) * rightFirst)

calculateAIScoreAtListHard :: Int -> [Cell] -> Int
calculateAIScoreAtListHard k list = do
    let leftList = reverse (take k list)
    let rightList = drop (k+1) list
    let leftConNum = max ((calculateContinueNum leftList) - 2) 0
    let rightConNum = max ((calculateContinueNum rightList) - 2) 0
    let leftFirst = getFirstEle leftList
    let rightFirst = getFirstEle rightList
    if (leftConNum == 4 && leftFirst == 1)
      || (rightConNum == 4 && rightFirst == 1) then
     10 ^ 10
    else
        (if (leftFirst == 2 && rightFirst == 2)
              || (leftFirst == 1 && rightFirst == 1) then
              10 ^ ((leftConNum + 1 + rightConNum) ^ 2)
          else
              (10 ^ ((leftConNum ^ 2) + 2) * leftFirst)
                + (10 ^ ((rightConNum ^ 2) + 2) * rightFirst))


getFirstEle :: [Cell] -> Int
getFirstEle [] = 0
getFirstEle [x]
    | x == Occ 1 = 3
    | x == Occ 2 = 1
    | otherwise = 0
getFirstEle (x : _)
  | x == Occ 1 = 3
  | x == Occ 2 = 1
  | otherwise = 0

calculateContinueNum :: [Cell] -> Int
calculateContinueNum  [] = 0
calculateContinueNum  [_] = 1
calculateContinueNum  [x1, x2] = if x1 == x2 then 2 else 1
calculateContinueNum  x@(x1:x2:_)  = if x1 == x2 then 1 + calculateContinueNum (tail x) else 1

test1 :: [[Cell]]
test1 = do
  let all0 = replicate 9 Empty
  let center1 = replicate 4 Empty ++ [Occ 1] ++ replicate 4 Empty
  replicate 4 all0 ++ [center1] ++ replicate 4 all0


test2 :: [[Cell]]
test2 = do
  let all0 = replicate 9 Empty
  let row1 = replicate 4 Empty ++ [Occ 1] ++ replicate 4 Empty
  let row2 = replicate 4 Empty ++ [Occ 1, Empty, Occ 2] ++ replicate 2 Empty
  let row3 = replicate 4 Empty ++ [Occ 1,  Occ 2] ++ replicate 3 Empty
  replicate 4 all0 ++ [row2, row3, row1] ++ replicate 2 all0

test3 :: [[Cell]]
test3 = do
  let all0 = replicate 9 Empty
  let row1 = replicate 4 Empty ++ [Occ 1] ++ replicate 4 Empty
  let row2 = replicate 3 Empty ++ [Occ 1] ++ replicate 5 Empty
  let row3 = replicate 2 Empty ++ [Occ 1] ++ replicate 6 Empty
  replicate 5 all0 ++ [row1, row2, row3] ++ replicate 1 all0 
