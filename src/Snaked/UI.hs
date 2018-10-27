{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Snaked.UI where

import           Control.Concurrent             ( threadDelay
                                                , forkIO
                                                )
import           Control.Lens            hiding ( Empty
                                                , center
                                                )
import           Control.Monad.State
import qualified Data.Set                      as S
import           Linear.V2

import           Data.Aeson
import           Brick.BChan
import           Brick                   hiding ( Direction )
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import qualified Graphics.Vty                  as V
import qualified Network.WebSockets            as WS

import           Snaked.Snake                   ( SnakeId(..) )
import           Snaked.GameState
import           Snaked.Grid
import           Data.Text                      ( Text )
import qualified Data.Text                     as T

body :: Widget ()
body = str "██"

open :: Widget ()
open = str "  "

data Piece = Body | Fruit | Empty

renderPiece Body  = body
renderPiece Empty = open

gameStateToGrid :: GameState -> [[Piece]]
gameStateToGrid ss@GameState {..} =
  let bodyParts = S.fromList $ ss ^.. allSnakesCoords
      (x', y')  = _size
  in  [ [ if S.member (mkCoord x y) bodyParts then Body else Empty
        | x <- [0 .. x' - 1]
        ]
      | y <- [0 .. y' - 1]
      ]

renderGameState :: GameState -> Widget ()
renderGameState ss =
  center
    $   withBorderStyle unicodeRounded
    $   border
    $   vBox
    $   hBox
    .   fmap renderPiece
    <$> gameStateToGrid ss

-- main :: IO ()
-- main = simpleMain $ renderGameState defaultGameState

type Name = ()

app :: WS.Connection -> App GameState GameState ()
app serverConn = App
  { appDraw         = (: []) . renderGameState
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent serverConn
  , appStartEvent   = return
  , appAttrMap      = const theMap
  }

theMap :: AttrMap
theMap = attrMap V.defAttr []

handleEvent
  :: WS.Connection
  -> GameState
  -> BrickEvent Name GameState
  -> EventM Name (Next GameState)
handleEvent _ ss (AppEvent newGameState) = handleTick newGameState
handleEvent serverConn ss (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt ss
handleEvent serverConn ui (VtyEvent (V.EvKey V.KUp [])) = do
  liftIO $ WS.sendTextData serverConn (encode S)
  continue ui
handleEvent serverConn ui (VtyEvent (V.EvKey V.KLeft [])) = do
  liftIO $ WS.sendTextData serverConn (encode W)
  continue ui
handleEvent serverConn ui (VtyEvent (V.EvKey V.KRight [])) = do
  liftIO $ WS.sendTextData serverConn (encode E)
  continue ui
handleEvent serverConn ui (VtyEvent (V.EvKey V.KDown [])) = do
  liftIO $ WS.sendTextData serverConn (encode N)
  continue ui
handleEvent serverConn ss _ = continue ss

handleTick :: GameState -> EventM Name (Next GameState)
handleTick ss = continue $ step ss

playGame :: WS.Connection -> IO GameState
playGame serverConn = do
  chan <- newBChan 10
  forkIO $ forever $ do
    Just game' <- decode <$> WS.receiveData serverConn
    writeBChan chan game'
  customMain (V.mkVty V.defaultConfig) (Just chan) (app serverConn) empty
