module Main where
import Game 
import FrontEnd
import qualified Brick.Main as M
import qualified Brick.BChan as BC
import qualified Graphics.Vty as V
import Control.Concurrent 
import Control.Concurrent.STM
import Control.Monad (forever)


initialBoard :: [Int]
initialBoard = replicate (9 * 9) 0
-- initialBoard = take (9 * 9) [0..]

dummyTimeLimit :: Int
dummyTimeLimit = 20


main :: IO ()
main = do
    -- add user input to set up game
    eventChan <- BC.newBChan dummyTimeLimit
    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    switch <- newTVarIO OFF
    forkIO $ forever $ tictocThread switch eventChan
    let game = mkGame initialBoard 1 dummyTimeLimit switch
    gameOver <- M.customMain initialVty buildVty 
                    (Just eventChan) app game
    -- gameOver <- M.defaultMain app game
    return ()
    -- M.simpleMain (drawUI game)
