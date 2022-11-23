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
focusPosition (Game _ (x, y) _) wboard = 
  wboard & ix y
         . ix x
         %~ withAttr styleFocus

drawCell :: Cell -> Widget ()
drawCell c = center $ case c of
  Occ i -> str $ show i
  Empty -> str " "

drawRow :: Row -> Widget ()
drawRow r = vBox $ fmap drawCell r

drawBoard :: Game -> Widget ()
-- drawBoard b = hBox $ fmap drawRow b
drawBoard g@(Game b _ _) = 
  fmap (fmap (drawCell)) b
  & focusPosition g
  & fmap (intersperse (withBorderStyle unicode vBorder))
  & fmap (hBox)
  & intersperse (withBorderStyle unicode hBorder)
  & vBox
  & border
  & withBorderStyle unicodeBold
  & setAvailableSize (73, 37)
  & padRight (Pad 1)

-- TODO: add countdown, instruction, etc.
drawUI :: Game -> [Widget ()]
drawUI game = [drawBoard game]


handleEvent :: BrickEvent () e -> EventM () Game ()
handleEvent (VtyEvent (V.EvKey k [])) = do
  game <- get
  put $ case k of
    V.KUp    -> moveCursor North game
    V.KDown  -> moveCursor South game
    V.KLeft  -> moveCursor West game
    V.KRight -> moveCursor East game
    _        -> game
  return ()

handleEvent _ = M.halt

app :: App Game e ()
app = App 
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return ()
  , appAttrMap      = const attributes
  }