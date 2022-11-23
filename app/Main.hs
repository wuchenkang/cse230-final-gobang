module Main where
import Game 
import FrontEnd
import qualified Brick.Main as M

initialBoard :: [Int]
initialBoard = replicate (9 * 9) 0

main :: IO ()
main = do
    let game = mkGame initialBoard 0
    M.simpleMain (drawUI game)
