module Game where

import Data.List (isInfixOf)
import Data.List.Split (chunksOf)
import Data.Function ((&))
import Lens.Micro
import Brick
import Brick.BChan
import Control.Concurrent 
import Control.Concurrent.STM
import Control.Monad.Trans (liftIO)
import qualified Brick.Main as M

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
  , mode :: Int -- 0:PVP 1:AI
  , status :: Status
  }

data Status = Playing | Win Int | Draw

mkGame :: [Int] -> Int -> Int -> TVar TimerStatus -> Game
mkGame ib p t s = Game 
  { 
    board = chunksOf 9 $ mkCell <$> ib
  , focusPos = (4, 4)
  , player = p
  , tictoc = t
  , timeLimit = t
  , timerStatus = s
  , mode = 0 -- NEED CHANGE, default PVP
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

winGame :: Game -> Bool
winGame game = rowHit || colHit || diagHit
  where
    rowHit = any (listHit p) (getRows game)
    colHit = any (listHit p) (getColumns game)
    diagHit = any (listHit p) (getDiagonals game)
    p = player game

listHit :: Int -> [Cell] -> Bool
listHit _ [] = False
listHit p xs =  (isInfixOf (take 5 (repeat $ Occ p)) xs) || listHit p (tail xs)

drawGame :: Game -> Bool
drawGame game = not $ any (== Empty) $ concat $ board game

detectState :: Game -> Game
detectState game = 
  if winGame game then game {status = Win $ player game}
  else if drawGame game then game {status = Draw}
  else timerUpdate (timeLimit game) game

changePlayer :: Game -> IO()
changePlayer game = do
  p <- player (switchPlayer game)
  g <- game {player = p}
  -- if(p==2) then (return (putAI g))
  -- else
  turnOnTimer g

switchPlayer :: Game -> Game
switchPlayer game
  | m==0 = game { player = 1-p }
  | m==1 = game { player = 2-p }
    where p = player game
          m = mode game


-- -- AI functions
-- putAI :: Game -> Game 

-- calculateAIScore :: Game -> [Int]

-- calculateAIScoreAt :: Game -> Int -> Int -> Int

-- calculateAIScoreAtList :: Game -> Int -> [Cell] -> Int


getColumnAt :: Game -> Int -> [Cell]
getColumnAt game j = [board game !! row !! j | row <- [0..8]] 

getRowAt :: Game -> Int -> [Cell]
getRowAt game i = board game !! i

getLeftDiagonalAt :: Game -> Int -> [Cell]
getLeftDiagonalAt game diff = [board game !! row !! col | row <- [0..8], col <- [0..8], row - col == diff]

getRightDiagonalAt :: Game -> Int -> [Cell]
getRightDiagonalAt game sum = [board game !! row !! col | row <- [0..8], col <- [0..8], row + col == sum]

getRows :: Game -> [[Cell]]
getRows game = [getRowAt game i | i <- [0..8]]

getColumns :: Game -> [[Cell]]
getColumns game = [getColumnAt game i | i <- [0..8]]

getDiagonals :: Game -> [[Cell]]
getDiagonals game = [getLeftDiagonalAt game i | i <- [-8..8]] ++ [getLeftDiagonalAt game i | i <- [0, 16]]
