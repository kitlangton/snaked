{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Snaked.UI where

import           Control.Concurrent             ( threadDelay
                                                , forkIO
                                                )
import qualified Brick.Main                    as M
import           Brick.Util                     ( fg
                                                , on
                                                )
import qualified Brick.AttrMap                 as A
import           Brick.Types                    ( Widget )
import           Brick.BChan


import           Snaked.Snake
import           Brick                   hiding ( Direction )
import           Snaked
import           Snaked.Grid
import           Control.Lens            hiding ( Empty )
import qualified Data.Set                      as S
import qualified Data.Set                      as S
import           Linear.V2
import qualified Brick.Widgets.Border.Style    as BorderStyle
import qualified Brick.Widgets.Border          as Border
import qualified Brick.Widgets.Center          as Center
import qualified Graphics.Vty                  as V
import           Control.Monad.State

body :: Widget ()
body = str "██"

empty :: Widget ()
empty = str "  "

data Piece = Body | Fruit | Empty

renderPiece Body  = body
renderPiece Empty = empty

snakeStateToGrid :: SnakeState -> [[Piece]]
snakeStateToGrid ss@SnakeState {..} =
  let bodyParts = S.fromList $ ss ^.. allSnakesCoords
      (x', y')  = _size
  in  [ [ if S.member (mkCoord x y) bodyParts then Body else Empty
        | x <- [0 .. x' - 1]
        ]
      | y <- [0 .. y' - 1]
      ]

renderSnakeState :: SnakeState -> Widget ()
renderSnakeState ss =
  Center.center
    $   withBorderStyle BorderStyle.unicodeRounded
    $   Border.border
    $   vBox
    $   hBox
    .   fmap renderPiece
    <$> snakeStateToGrid ss

-- main :: IO ()
-- main = simpleMain $ renderSnakeState defaultSnakeState

data Tick = Tick
type Name = ()

app :: App SnakeState Tick ()
app = App
  { appDraw         = (: []) . renderSnakeState
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const theMap
  }

theMap :: AttrMap
theMap = attrMap V.defAttr []

handleEvent
  :: SnakeState -> BrickEvent Name Tick -> EventM Name (Next SnakeState)
handleEvent ss (AppEvent Tick) = handleTick ss
handleEvent ss (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt ss
handleEvent ui (VtyEvent (V.EvKey V.KUp [])) = handleTurn S ui
handleEvent ui (VtyEvent (V.EvKey V.KLeft [])) = handleTurn W ui
handleEvent ui (VtyEvent (V.EvKey V.KRight [])) = handleTurn E ui
handleEvent ui (VtyEvent (V.EvKey V.KDown [])) = handleTurn N ui
handleEvent ss _ = continue ss

handleTick :: SnakeState -> EventM Name (Next SnakeState)
handleTick ss = continue $ execState step ss

handleTurn :: Direction -> SnakeState -> EventM Name (Next SnakeState)
handleTurn dir ss = continue $ execState (turn (SnakeId 1) dir) ss

main = defaultMain app defaultSnakeState

playGame :: IO SnakeState
playGame = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000
  customMain (V.mkVty V.defaultConfig) (Just chan) app defaultSnakeState
