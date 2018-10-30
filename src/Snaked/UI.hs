{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Snaked.UI where

import           Control.Concurrent             ( forkIO )
import           Control.Monad.State
import qualified Data.Map                      as M
import           Data.Aeson
import           Brick.BChan
import           Brick                   hiding ( Direction )
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import qualified Graphics.Vty                  as V
import qualified Network.WebSockets            as WS
import           Snaked.GameState
import           Snaked.Grid
import           Snaked.Snake
import           Control.Lens            hiding ( Empty )

body :: SnakeId -> Widget ()
body sid = withAttr (snakeColor sid) $ str "██"

open :: Widget ()
open = str "  "

fruit :: Widget ()
fruit = withAttr "fruit" $ str "██"

renderPiece :: RenderCoord -> Widget ()
renderPiece (RBody sid) = body sid
renderPiece REmpty      = open
renderPiece RFood       = fruit

renderGameState :: RenderState -> Widget ()
renderGameState ss =
  center
    $   withBorderStyle unicodeRounded
    $   border
    $   vBox
    $   hBox
    .   fmap renderPiece
    <$> ss

type Name = ()

app :: WS.Connection -> App RenderState RenderState ()
app serverConn = App
  { appDraw         = (: []) . renderGameState
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent serverConn
  , appStartEvent   = return
  , appAttrMap      = const theMap
  }


blue, green, red, yellow :: AttrName
blue = "blue"
green = "green"
red = "red"
yellow = "yellow"

snakeColor :: SnakeId -> AttrName
snakeColor sid = [blue, green, yellow, red] !! fromIntegral sid

theMap :: AttrMap
theMap = attrMap
  V.defAttr
  [ ("fruit", fg V.white)
  , (blue   , fg V.blue)
  , (green  , fg V.green)
  , (red    , fg V.red)
  , (yellow , fg V.yellow)
  ]

keyToDir :: V.Key -> Maybe Direction
keyToDir V.KUp    = Just S
keyToDir V.KLeft  = Just W
keyToDir V.KRight = Just E
keyToDir V.KDown  = Just N
keyToDir _        = Nothing

handleEvent
  :: WS.Connection
  -> RenderState
  -> BrickEvent Name RenderState
  -> EventM Name (Next RenderState)
handleEvent _ _ (AppEvent newGameState) = continue newGameState
handleEvent _ ss (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt ss
handleEvent serverConn ui (VtyEvent (V.EvKey (keyToDir -> Just key) [])) = do
  liftIO $ WS.sendTextData serverConn (encode key)
  continue ui
handleEvent _ ss _ = continue ss

playGame :: WS.Connection -> IO RenderState
playGame serverConn = do
  chan <- newBChan 10
  _    <- forkIO $ forever $ do
    Just game' <- decode <$> WS.receiveData serverConn
    writeBChan chan game'
  customMain (V.mkVty V.defaultConfig) (Just chan) (app serverConn) []
