module Types where

import Network.Socket ( Socket )
import Control.Concurrent.STM ( TVar )
import Brick.BChan ( BChan )

data State = SelectMode | SelectInitiative | SelectDiff
  deriving (Eq, Show)

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
data Difficulty = Easy | Hard deriving (Eq, Show)

data Game = Game 
  { board :: Board
  , focusPos :: (Int, Int)
  , player :: Int -- Current player
  , identity :: Int -- 0: host, 1: client
  , tictoc :: Int
  , timeLimit :: Int
  , timerStatus :: TVar TimerStatus
  , mode :: Mode
  , status :: Status
  , gchan :: BChan GobangEvent
  , msock :: Maybe Socket -- Only used in online mode
  , difficulty :: Difficulty -- Only used in AI mode
  }

data Setup = Setup
  { state :: State
  , typ :: Int
  , initiative :: Int
  , diff :: Int
  , ip :: String
  , iden :: Int
  }

data CursorDirection
  = North
  | South
  | East
  | West
  deriving (Eq, Show)