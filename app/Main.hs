module Main where
import Game
import FrontEnd
import NetUtil
import qualified Brick.Main as M
import qualified Brick.BChan as BC
import qualified Graphics.Vty as V
import Control.Concurrent ( forkIO ) 
import Control.Concurrent.STM ( newTVarIO )
import Control.Monad (forever)
import Game (Difficulty(Easy, Hard))

initialBoard :: [Int]
initialBoard = replicate (9 * 9) 0
-- initialBoard = take (9 * 9) [0..]

dummyTimeLimit :: Int
dummyTimeLimit = 10


main :: IO ()
main = do
    setup <- M.defaultMain  setupApp Setup {
        state=SelectMode,
        typ=0,
        initiative=1,
        difficulty=1,
        ip="192.168.1.1"
    }
    eventChan <- BC.newBChan dummyTimeLimit
    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    switch <- newTVarIO OFF
<<<<<<< HEAD
    forkIO $ forever $ tictocThread switch eventChan
    let game = mkGame setup initialBoard dummyTimeLimit switch
=======

    -- add timer thread
    _ <- forkIO $ forever $ tictocThread switch eventChan

    -- TODO: game from panel
    let game = mkGame AI initialBoard 1 dummyTimeLimit switch eventChan Hard

    -- start server or client thread
    case mode game of
        Local      -> putStrLn "Playing local pvp"
        AI         -> putStrLn "Playing with AI"
        (Online 0) -> do
            putStrLn "Starting server ..."
            sock <- createRoom
            _ <- forkIO $ forever $ waitForPlacement sock eventChan
            return ()
        (Online 1) -> do
            putStrLn "Joining game ..."
            sock <- joinGame
            return ()
        _ -> return ()

>>>>>>> 675811fd327add52e563cf71af2a1e5d2e8b6fa6
    gameOver <- M.customMain initialVty buildVty 
                    (Just eventChan) app game
    -- gameOver <- M.defaultMain app game
    return ()
    -- M.simpleMain (drawUI game)
