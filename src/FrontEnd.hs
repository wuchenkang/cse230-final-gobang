module FrontEnd where

import Game
import Data.Function ((&))
import Lens.Micro
import Brick 
import qualified Brick.Main as M
import Brick.Widgets.Border (border, borderWithLabel, hBorderWithLabel, vBorder, hBorder)
import Brick.Widgets.Border.Style (unicode, unicodeBold)
import Brick.Widgets.Center (center)
import Data.Char (digitToInt)
import Data.List (intersperse)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V
import Control.Concurrent 
import Control.Concurrent.STM
import Control.Monad.Trans (liftIO)


stylePlayer1, stylePlayer2, styleHigh, styleMid, styleLow, styleFocus :: AttrName
stylePlayer1 = attrName "stylePlayer1"
stylePlayer2 = attrName "stylePlayer2"
styleHigh    = attrName "styleHigh"
styleMid     = attrName "styleMid"
styleLow     = attrName "styleLow"
styleFocus   = attrName "styleFocus"

attributes :: AttrMap
attributes = attrMap V.defAttr 
  [ (stylePlayer1, fg V.white)
  , (stylePlayer2, fg V.black)
  , (styleHigh,    fg V.green)
  , (styleMid,     fg V.yellow)
  , (styleLow,     fg V.red)
  , (styleFocus,   bg V.cyan) 
  ]

focusPosition :: Game -> [[Widget ()]] -> [[Widget ()]]
focusPosition game wboard = 
  wboard & ix y
         . ix x
         %~ withAttr styleFocus
  where (x, y) = focusPos game

drawCell :: Cell -> Widget ()
drawCell c = center $ case c of
  Occ 1 -> withAttr stylePlayer1 $ str "⬤" 
  Occ 2 -> withAttr stylePlayer2 $ str "⬤" 
  Empty -> str " "

drawRow :: Row -> Widget ()
drawRow r = vBox $ fmap drawCell r

drawBoard :: Game -> Widget ()
-- drawBoard b = hBox $ fmap drawRow b
drawBoard game = 
  fmap (fmap (drawCell)) (board game)
  & focusPosition game
  & fmap (intersperse (withBorderStyle unicode vBorder))
  & fmap (hBox)
  & intersperse (withBorderStyle unicode hBorder)
  & vBox
  & border
  & withBorderStyle unicodeBold
  & setAvailableSize (73, 37)
  & padRight (Pad 1)

drawHelp :: Widget ()
drawHelp =
  [ "move:    ←↓↑→ / wasd "
  , "place:   Enter"
  , "quit:    q"
  ]
  & unlines
  & str
  & padRight Max
  & padLeftRight 1
  & borderWithLabel (str " Help ")
  & withBorderStyle unicodeBold
  & setAvailableSize (31, 12)

drawTimer :: Game -> Widget ()
drawTimer game = 
  [ show $ c `div` 60
  , " : " 
  , show $ c `mod` 60
  ]
  & fmap str
  & hBox
  & padRight Max
  & padLeftRight 1
  & borderWithLabel (str " Time Left ")
  & withAttr sufficient
  & withBorderStyle unicodeBold
  & hLimit 31
  where 
    c = tictoc game
    sufficient | c >= l `div` 2 = styleHigh
               | c >= l `div` 4 = styleMid
               | otherwise      = styleLow
    l = timeLimit game

-- TODO: add countdown, instruction, etc.
drawUI :: Game -> [Widget ()]
drawUI game = [draw]
  where 
   draw = drawBoard game <+> ( drawHelp
                         <=>   drawTimer game
                             )

handleGameEvent :: BrickEvent () e -> EventM () Game ()
handleGameEvent (VtyEvent (V.EvKey k [])) = do
  case k of
    V.KUp    -> modify $ moveCursor North
    V.KDown  -> modify $ moveCursor South
    V.KLeft  -> modify $ moveCursor West
    V.KRight -> modify $ moveCursor East
  
    V.KChar 'w' -> modify $ moveCursor North
    V.KChar 's' -> modify $ moveCursor South
    V.KChar 'a' -> modify $ moveCursor West
    V.KChar 'd' -> modify $ moveCursor East
  
    V.KEnter -> do
      game <- get
      liftIO $ turnOffTimer game
      modify $ placePiece . (timerUpdate $ timeLimit game)
    V.KChar 'q' -> M.halt
    _ -> return ()
handleGameEvent _ = return ()

-- TODO: Time out to AI / PVP
handleEvent :: BrickEvent () GobangEvent -> EventM () Game ()
handleEvent (AppEvent (Placement (x, y))) = return ()
handleEvent (AppEvent Countdown) = do
  game <- get
  s <- liftIO $ readTVarIO $ (timerStatus game)
  case s of
    ON -> do
      if ((tictoc game) > 0)
        then modify $ timerUpdate $ (tictoc game) - 1
        else M.halt -- Timeout
    -- conume dead countdown if any
    _ -> return ()
handleEvent e = handleGameEvent e

initializeEvent :: EventM () Game ()
initializeEvent = do
  game <- get
  liftIO $ turnOnTimer game

app :: App Game GobangEvent ()
app = App 
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = initializeEvent
  , appAttrMap      = const attributes
  }