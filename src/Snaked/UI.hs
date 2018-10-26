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

import           Brick.BChan
import           Brick                   hiding ( Direction )
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import qualified Graphics.Vty                  as V

import           Snaked.Snake                   ( SnakeId(..) )
import           Snaked.GameState
import           Snaked.Grid

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

data Tick = Tick
type Name = ()

app :: App GameState Tick ()
app = App
  { appDraw         = (: []) . renderGameState
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const theMap
  }

theMap :: AttrMap
theMap = attrMap V.defAttr []

handleEvent :: GameState -> BrickEvent Name Tick -> EventM Name (Next GameState)
handleEvent ss (AppEvent Tick) = handleTick ss
handleEvent ss (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt ss
handleEvent ui (VtyEvent (V.EvKey V.KUp [])) = handleTurn S ui
handleEvent ui (VtyEvent (V.EvKey V.KLeft [])) = handleTurn W ui
handleEvent ui (VtyEvent (V.EvKey V.KRight [])) = handleTurn E ui
handleEvent ui (VtyEvent (V.EvKey V.KDown [])) = handleTurn N ui
handleEvent ss _ = continue ss

handleTick :: GameState -> EventM Name (Next GameState)
handleTick ss = continue $ step ss

handleTurn :: Direction -> GameState -> EventM Name (Next GameState)
handleTurn dir ss = continue $ (intendTurn (SnakeId 1) dir) ss

playGame :: IO GameState
playGame = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000
  customMain (V.mkVty V.defaultConfig) (Just chan) app empty
