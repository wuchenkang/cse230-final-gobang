module AI where

import Data.List (isInfixOf)
import Data.List.Split (chunksOf)
import Lens.Micro
import Brick
import Brick.BChan
import Control.Concurrent 
import Control.Concurrent.STM
import Control.Monad.Trans (liftIO)
import Game


-- -- AI functions
-- putAI :: Game -> Game 

-- calculateAIScore :: Game -> [Int]

-- calculateAIScoreAt :: Game -> Int -> Int -> Int

-- calculateAIScoreAtList :: Game -> Int -> [Cell] -> Int