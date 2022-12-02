module Game where

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

data Cell
  = Occ Int
  | Empty
  deriving (Eq, Show)

type Row = [Cell]
type Board = [Row]

data GobangEvent = 
  Countdown | Placement (Int, Int)

data TimerStatus = ON | OFF deriving (Eq, Show)

data Game = Game 
  { board :: Board
  , focusPos :: (Int, Int)
  , player :: Int -- 1 P1, (P2, AI)
  , identity :: Int -- 1 P1, (P2, AI), will never change
  , tictoc :: Int -- TODO: timer
  , timeLimit :: Int
  , timerStatus :: TVar TimerStatus
  , mode :: Mode
  , status :: Status
  , sock :: Maybe Socket
  }

data Mode = Local | AI | Online Int -- 0: host, 1: customer
  deriving (Eq, Show)

data Status = Playing | Win Int | Draw deriving (Eq, Show)

mkGame :: Mode -> [Int] -> Int -> Int -> TVar TimerStatus -> Maybe Socket -> Game
mkGame m ib p t s sock = Game 
  { 
    board = chunksOf 9 $ mkCell <$> ib
  , focusPos = (4, 4)
  , player = p
  , identity = p
  , tictoc = t
  , timeLimit = t
  , timerStatus = s
  , mode = m
  , status = Playing
  , sock = sock
  }
  where
    mkCell 0 = Empty
    mkCell i = Occ i
  


data CursorDirection
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

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

afterPlacement :: EventM () Game ()
afterPlacement = do
  modify detectState
  game <- get
  case status game of 
    Win _ -> return ()
    Draw  -> return ()
    _     -> do
      -- switch player
      let game' = switchPlayer game
      -- reset and turn on/off timer
      let game'' = timerUpdate (timeLimit game) game'
      if mode game'' == Local
        then liftIO $ turnOnTimer game'' 
        else liftIO $ turnOffTimer game''
      put game''
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
getDiagonals game = [getLeftDiagonalAt (board game) i | i <- [-8..8]] ++ [getLeftDiagonalAt (board game) i | i <- [0, 16]]


-- AI Section
putAI :: Game -> Game
putAI game = do
    let boardNow = board game
    let scores = calculateAIScore boardNow
    let (_, xs) = maximumBy (\x y -> compare (fst x) (fst y)) (zip scores [0..])
    let (i, j) = getIJfromIndex xs
    placePiece game i j

putAITesting :: [[Cell]] -> (Int, Int)
putAITesting boardNow = do
    let scores = calculateAIScore boardNow
    let (_, xs) = maximumBy (\x y -> compare (fst x) (fst y)) (zip scores [0..])
    getIJfromIndex xs
    


getIJfromIndex :: Int -> (Int, Int)
getIJfromIndex k = (i, j)
    where 
        i = k `div` 9
        j = k `mod` 9


calculateAIScore :: [[Cell]] -> [Int]
calculateAIScore game = [calculateAIScoreAt game row col | row <- [0..8], col <- [0..8]]

calculateAIScoreAt :: [[Cell]] -> Int -> Int -> Int
calculateAIScoreAt game i j = do
    let item = game !! i !! j
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
    if item == Empty then score1 + score2 + score3 + score4 else 0


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
getFirstEle (x : _)
  | x == Occ 0 = 2
  | x == Occ 1 = 1
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

