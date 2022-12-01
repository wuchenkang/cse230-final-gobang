module Game where

import Data.List.Split (chunksOf)
import Data.Function ((&))
import Lens.Micro
import Brick
import Brick.BChan
import Control.Concurrent 
import Control.Concurrent.STM

data Cell
  = Occ Int
  | Empty
  deriving (Eq, Show)

type Row = [Cell]
type Board = [Row]

data GobangEvent = 
  Countdown | Placement (Int, Int)

data TimerStatus = ON | OFF

data Game = Game 
  { board :: Board
  , focusPos :: (Int, Int)
  , player :: Int
  , tictoc :: Int -- TODO: timer
  , timeLimit :: Int
  , timerStatus :: TVar TimerStatus
  } 

mkGame :: [Int] -> Int -> Int -> TVar TimerStatus -> Game
mkGame ib p t s = Game 
  { 
    board = chunksOf 9 $ mkCell <$> ib
  , focusPos = (4, 4)
  , player = p
  , tictoc = t
  , timeLimit = t
  , timerStatus = s
  }
  where
    mkCell 0 = Empty
    mkCell i = Occ i

data CursorDirection
  = North
  | South
  | East
  | West

moveCursor :: CursorDirection -> Game -> Game
moveCursor d g = 
  move d (focusPos g) 
  & (\p -> g { focusPos = p })
  where
    move :: CursorDirection -> (Int, Int) -> (Int, Int)
    move North (x, y) = (x, check $ y - 1)
    move South (x, y) = (x, check $ y + 1)
    move East  (x, y) = (check $ x + 1, y)
    move West  (x, y) = (check $ x - 1, y)
    check x | x < 0     = 0
            | x > 8     = 8
            | otherwise = x

switchPlayer :: Game -> Game
switchPlayer game = game { player = 1 - p }
  where p = player game

placePiece :: Game -> Game
placePiece game = game { board = board game & ix y . ix x .~ p }
  where
    (x, y) = focusPos game
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
  s <- atomically $ readTVar status
  case s of
    ON -> do 
      threadDelay 1000000
      writeBChan chan Countdown
    _ -> do
      threadDelay 1000000
-- AI functions
putAI :: Game -> Game 


calculateAIScore :: Game -> [Int]

calculateAIScoreAt :: Game -> Int -> Int -> Int


calculateAIScoreAtList :: Int -> [Cell] -> Int
calculateAIScoreAtList k list = do
  let leftList = reverse  (take k list)
  let rightList = drop k+1 list
  let leftConNum = calculateContinueNum leftList
  let rightConNum = calculateContinueNum RightList
  let leftFirst = getFirstEle leftList
  let rightFirst = getFirstEle rightFirst




getFirstEle :: [Cell] -> Int
getFirstEle [] = Empty
getFirstEle [x] = if x == Occ 0 then 2 else if x == Empty else 1
getFirstEle (x:xs) = if x == Occ 0 then 2 else 1

calculateContinueNum :: [Cell] -> Int
calculateContinueNum  [] = 0
calculateContinueNum  (x1:x2) = if x1 == x2 then 2 else 0
calculateContinueNum  x@(x1:x2:xs)  = if x1 == x2 then (1 + calculateContinueNum (tail x)) else 0



getColumnAt :: Game -> Int -> [Cell]
getColumnAt game j = [board game !! row !! j | row <- [0..8]] 

getRowAt :: Game -> Int -> [Cell]
getRowAt game i = board game !! i

getLeftDiagonalAt :: Game -> Int -> [Cell]
getLeftDiagonalAt game diff = [board game !! row !! col | row <- [0..8], col <- [0..8], row - col == diff]

getRightDiagonalAt :: Game -> Int -> [Cell]
getRightDiagonalAt game sum = [board game !! row !! col | row <- [0..8], col <- [0..8], row + col == sum]