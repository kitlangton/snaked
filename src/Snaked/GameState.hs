{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Snaked.GameState where

import           Snaked.Grid
import           Control.Lens
import           Control.Lens.TH
import           Control.Monad.State
import qualified Data.Map.Strict               as M
import           Data.Aeson.TH
import           Data.Maybe
import           Linear.V2
import           Snaked.Snake
import qualified Snaked.Snake                  as Snake

type SnakeMap = M.Map SnakeId Snake

data GameState = GameState
  { _snakes        :: SnakeMap
  , _size          :: Size
  , _foodLocations :: [Coord]
  } deriving (Show)

type SnakeT a = State GameState a

$(makeLenses ''GameState)

$(deriveJSON defaultOptions ''Direction)

data RenderCoord
  = RBody SnakeId
  | RFood
  | REmpty
  deriving (Show, Eq)

$(deriveJSON defaultOptions ''RenderCoord)

type RenderState = [[RenderCoord]]

renderableState :: GameState -> RenderState
renderableState gs =
  [ [ fromMaybe REmpty $ M.lookup (V2 x y) renderMap | x <- [0 .. x' - 1] ]
  | y <- [0 .. y' - 1]
  ]
 where
  (x', y')  = gs ^. size
  renderMap = stateRenderMap gs

type RenderMap = M.Map Coord RenderCoord

stateRenderMap :: GameState -> RenderMap
stateRenderMap gs = snakeRenderMaps gs <> foodRenderMap gs
 where
  snakeRenderMaps :: GameState -> RenderMap
  snakeRenderMaps GameState {..} = foldMap snakeRenderMap _snakes
  snakeRenderMap Snake {..} = M.fromList $ (, RBody _snakeId) <$> _pieces
  foodRenderMap gs = M.singleton (gs ^. foodCoord) RFood

empty :: GameState
empty = GameState (M.fromList []) (20, 20) (randomCoords (20, 20))

foodCoord :: Getter GameState Coord
foodCoord = foodLocations . to head

addSnake :: SnakeId -> GameState -> GameState
addSnake sid =
  snakes %~ M.insert sid (Snake.fromList sid [(10, 11), (10, 10), (10, 9)])

removeSnake :: SnakeId -> GameState -> GameState
removeSnake sid = snakes %~ M.delete sid

allSnakeCoords :: GameState -> [Coord]
allSnakeCoords gs = gs ^.. snakes . traverse . pieces . traverse

step :: GameState -> GameState
step = advanceSnakes . checkFood . removeColliding

intendTurn :: SnakeId -> Direction -> GameState -> GameState
intendTurn snakeId newDirection gs =
  gs & snakes . ix snakeId %~ Snake.intendTurn newDirection

-- NEATO: (regarding where clause)
-- With lenses you can return both the updated structure as well as the
-- modified value
checkFood :: GameState -> GameState
checkFood gs | null eatingSnakes = gs
             | otherwise         = setNextFood gs'
 where
  food = gs ^. foodCoord
  (eatingSnakes, gs') =
    gs
      &   snakes
      .   partsOf (traverse . filtered (isEating food))
      <%~ fmap Snake.eat

setNextFood :: GameState -> GameState
setNextFood gs =
  gs & foodLocations %~ dropWhile (`coordOverlapsSnakes` gs) . tail

coordOverlapsSnakes :: Coord -> GameState -> Bool
coordOverlapsSnakes coord (allSnakeCoords -> snakeCoords) =
  coord `elem` snakeCoords

-- Advances snakes in their current directions
advanceSnakes :: GameState -> GameState
advanceSnakes gs =
  gs & snakes . traverse %~ (Snake.advance (gs ^. size) . Snake.finalizeTurn)

-- Removes colliding snakes
removeColliding :: GameState -> GameState
removeColliding = snakes %~ removeColliding'
 where
  removeColliding' :: SnakeMap -> SnakeMap
  removeColliding' snakeMap =
    M.filter (\s -> not $ any (Snake.colliding s) snakeMap) snakeMap
