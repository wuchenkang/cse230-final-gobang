module Types where

import Network.Socket ( Socket )
import Control.Concurrent.STM ( TVar )
import Brick.BChan ( BChan )

data Cell
  = Occ Int
  | Empty
  deriving (Eq, Show)

type Row = [Cell]
type Board = [Row]

data GobangEvent = 
  Countdown | Placement (Int, Int)

data TimerStatus = ON | OFF deriving (Eq, Show)

data Mode = Local | AI | Online Int -- 0: host, 1: customer
  deriving (Eq, Show)

data Status = Playing | Win Int | Draw deriving (Eq, Show)

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
  , gchan :: BChan GobangEvent
  , msock :: Maybe Socket
  }

data CursorDirection
  = North
  | South
  | East
  | West
  deriving (Eq, Show)