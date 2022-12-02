module FrontEnd where

import Game
import Lens.Micro
import Brick 
import qualified Brick.Main as M
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import Brick.Util (on, bg)
import qualified Brick.Types as T
import Brick.Widgets.Border (border, borderWithLabel, hBorderWithLabel, vBorder, hBorder)
import Brick.Widgets.Border.Style (unicode, unicodeBold, unicodeRounded)
import Brick.Widgets.Center (center)
import Data.List (intersperse)
import qualified Graphics.Vty as V
import Control.Concurrent.STM
import Control.Monad.Trans (liftIO)

title :: String
title = "\n\
\  ____       _\n\
\ / ___| ___ | |__   __ _ _ __   __ _\n\
\| |  _ / _ \\| '_ \\ / _` | '_ \\ / _` |\n\
\| |_| | (_) | |_) | (_| | | | | (_| |\n\
\ \\____|\\___/|_.__/ \\__,_|_| |_|\\__, |\n\
\                               |___/\n\
\"

drawSelectMode :: Setup -> Widget ()
drawSelectMode setup =
        str title
    <=> (if typ setup == 0 then withAttr styleFocus (str "1. Local  PVP") else str "1. Local  PVP")
    <=> (if typ setup == 1 then withAttr styleFocus (str "2. Local  PVE") else str "2. Local  PVE")
    <=> (if typ setup == 2 then withAttr styleFocus (str "3. Online PVP") else str "3. Online PVP")

drawSelectInitiative :: Setup -> Widget ()
drawSelectInitiative setup =
        str title
    <=> (if initiative setup == 1 then withAttr styleFocus (str "1. You first") else str "1. You first")
    <=> (if initiative setup == 2 then withAttr styleFocus (str "2. Opponent first") else str "2. Opponent first")

drawSetupUI :: Setup -> [Widget ()]
drawSetupUI setup@Setup{state=SelectMode} = [drawSelectMode setup]
drawSetupUI setup@Setup{state=SelectInitiative} = [drawSelectInitiative setup]

moveSetupTyp :: CursorDirection -> Setup -> Setup
moveSetupTyp North setup =  setup {typ = (typ setup - 1) `mod` 3}
moveSetupTyp South setup =  setup {typ = (typ setup + 1) `mod` 3}
moveSetupTyp _ setup = setup

moveSetupInitiative :: CursorDirection -> Setup -> Setup
moveSetupInitiative North setup =  setup {initiative = initiative setup `mod` 2 + 1}
moveSetupInitiative South setup =  setup {initiative = initiative setup `mod` 2 + 1}
moveSetupInitiative _ setup = setup

handleSetupEvent :: BrickEvent () e -> EventM () Setup ()
handleSetupEvent (VtyEvent (V.EvKey k [])) = do
  setup <- get
  case state setup of
    SelectMode ->
      case k of
          V.KUp -> modify $ moveSetupTyp North
          V.KDown -> modify $ moveSetupTyp South
          V.KEnter -> modify $ \s -> s {state = SelectInitiative}
          _ -> return ()
    SelectInitiative ->
      case k of
          V.KUp -> modify $ moveSetupInitiative North
          V.KDown -> modify $ moveSetupInitiative South
          V.KEnter -> M.halt
          _ -> return ()
handleSetupEvent _ = return ()


setupApp :: M.App (Setup) e ()
setupApp =
    M.App { M.appDraw = drawSetupUI
          , M.appChooseCursor = M.neverShowCursor
          , M.appHandleEvent = handleSetupEvent
          , M.appStartEvent = return ()
          , M.appAttrMap = const attributes
          }

stylePlayer1, stylePlayer2, styleHigh, styleMid, styleLow, styleFocus, styleChessBoard :: AttrName
stylePlayer1 = attrName "stylePlayer1"
stylePlayer2 = attrName "stylePlayer2"
styleHigh    = attrName "styleHigh"
styleMid     = attrName "styleMid"
styleLow     = attrName "styleLow"
styleFocus   = attrName "styleFocus"
styleChessBoard = attrName "styleChessBoard"

attributes :: AttrMap
attributes = attrMap V.defAttr 
  [ (stylePlayer1, fg V.white)
  , (stylePlayer2, fg V.black)
  , (styleHigh,    fg V.green)
  , (styleMid,     fg V.yellow)
  , (styleLow,     fg V.red)
  , (styleFocus,   bg V.cyan) 
  , (styleChessBoard, bg $ V.rgbColor 243 200 133)
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
  _     -> str "wtf"

drawRow :: Row -> Widget ()
drawRow r = vBox $ fmap drawCell r

drawBoard :: Game -> Widget ()
-- drawBoard b = hBox $ fmap drawRow b
drawBoard game = 
  fmap (fmap drawCell) (board game)
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
  [ "Move:    ←↓↑→ / WASD "
  , "Place:   Enter"
  , "Quit:    Q"
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
    str timestr
  & padLeftRight  (14 - (length timestr `div` 2))
  & borderWithLabel (str " Time Left ")
  & withAttr sufficient
  & withBorderStyle unicodeBold
  & hLimit 31
  where 
    c = tictoc game
    m = c `div` 60
    s = c `mod` 60
    minstr = if m < 10 then "0" ++ show m else show m
    secstr = if s < 10 then "0" ++ show s else show s
    timestr = minstr ++ " : " ++ secstr
    sufficient | c >= l `div` 2 = styleHigh
               | c >= l `div` 4 = styleMid
               | otherwise      = styleLow
    l = timeLimit game
  
drawTerm :: Game -> Widget ()
drawTerm game = 
    str name 
  & padLeftRight  (14 - (length name `div` 2))
  -- & padRight Max
  & borderWithLabel (str " Current Term ")
  & withBorderStyle unicodeBold
  & hLimit 31
  where name | player game == 1 = "Player 1"
             | player game == 2 && mode game /= AI = "Player 2"
             | otherwise        = "AI"  

drawWinner :: Game -> Status -> Widget ()
drawWinner _ (Win x) = str $ show name ++ " Won!"
    where name | x == 1 = "Player 1"
               | x == 2 = "Player 2"
               | otherwise = "AI"
drawWinner _ Draw = str $ show "Draw!"
drawWinner _ _ = str "impossible to happen"

drawDraw :: Widget ()
drawDraw = str "Draw"

drawNormal :: Game -> Widget ()
drawNormal game = (drawBoard game & withAttr styleChessBoard) 
                         <+> ( drawHelp
                         <=>   drawTimer game
                         <=>   drawTerm game
                             )

drawUI :: Game -> [Widget ()]
drawUI g@Game{status=Playing} = [drawNormal g]
drawUI Game{status=Draw}      = [drawDraw]
drawUI g@Game{status=Win _}   = [drawWinner g $ status g]

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
      if player game /= identity game
        then return ()
        else do
          liftIO $ turnOffTimer game
          modify placeFocus
          afterPlacement
    V.KChar 'q' -> M.halt
    _ -> return ()
handleGameEvent _ = return ()

-- TODO: Time out to AI / PVP
handleEvent :: BrickEvent () GobangEvent -> EventM () Game ()
handleEvent (AppEvent (Placement (x, y))) = do
  modify (\g -> placePiece g x y)
  afterPlacement
handleEvent (AppEvent Countdown) = do
  game <- get
  s <- liftIO $ readTVarIO $ timerStatus game
  case s of
    ON -> do
      if tictoc game > 0
        then modify $ timerUpdate $ tictoc game - 1
        else do
          modify $ randomPlace
          afterPlacement
    -- conume dead countdown if any
    -- endgame
    _ -> return ()
handleEvent e = handleGameEvent e

initializeEvent :: EventM () Game ()
initializeEvent = do
  game <- get
  liftIO $ turnOnTimer game

app :: App Game GobangEvent ()
app = App 
  { appDraw         = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = initializeEvent
  , appAttrMap      = const attributes
  }