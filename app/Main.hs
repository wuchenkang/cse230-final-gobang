module Main where

import Types
import Game 
import FrontEnd
import NetUtil
import qualified Brick.Main as M
import qualified Brick.BChan as BC
import qualified Graphics.Vty as V
import Control.Concurrent ( forkIO ) 
import Control.Concurrent.STM ( newTVarIO )
import Control.Monad (forever)

initialBoard :: [Int]
initialBoard = replicate (9 * 9) 0
-- initialBoard = take (9 * 9) [0..]

dummyTimeLimit :: Int
dummyTimeLimit = 10

main :: IO ()
main = do
    -- add user input to set up game
    eventChan <- BC.newBChan dummyTimeLimit
    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    switch <- newTVarIO OFF

    -- add timer thread
    _ <- forkIO $ forever $ tictocThread switch eventChan

    -- TODO: game from panel
    let game = mkGame (Online 0) initialBoard 1 dummyTimeLimit switch eventChan Nothing Easy :: Game

    -- start server or client thread
    game' <- setSockGameState game

    gameOver <- M.customMain initialVty buildVty 
                    (Just eventChan) app game'
    -- gameOver <- M.defaultMain app game
    return ()
    -- M.simpleMain (drawUI game)
