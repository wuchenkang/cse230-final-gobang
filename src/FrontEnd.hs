module FrontEnd where

import Game
import Brick 
import Brick.Widgets.Border (border, borderWithLabel, hBorderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode, unicodeBold)
import Brick.Widgets.Center (center)
import Data.Char (digitToInt)
import Data.List (intersperse)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V

drawCell :: Cell -> Widget ()
drawCell c = center $ case c of
  Occ i -> str $ show i
  Empty -> str "0"

drawRow :: Row -> Widget ()
drawRow r = vBox $ fmap drawCell r

drawBoard :: Board -> Widget ()
drawBoard b = hBox $ fmap drawRow b

-- TODO: add countdown, instruction, etc.
drawUI :: Game -> Widget ()
drawUI game = drawBoard $ board game


-- app :: App Game e ()
-- app = App {
--   appDraw = drawUI
--   app
-- }