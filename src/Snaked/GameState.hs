{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}

module Snaked.GameState where

import           Snaked.Grid

import           Control.Lens.TH
import           Control.Lens
import           Control.Monad.State
import qualified Data.Map.Strict               as M

import           Snaked.Snake
import qualified Snaked.Snake                  as Snake
import           Data.Aeson.TH
import           Linear.V2

type SnakeMap = M.Map SnakeId Snake

data GameState = GameState {
  _snakes :: SnakeMap,
  _size :: Size,
  _foodLocations :: [Coord]
} deriving Show

type SnakeT a = State GameState a

$(makeLenses ''GameState)
$(deriveJSON defaultOptions ''V2)
$(deriveJSON defaultOptions ''Direction)
$(deriveJSON defaultOptions ''Snake)
$(deriveJSON defaultOptions ''GameState)

empty = GameState (M.fromList []) (20, 20) (take 30 $ randomCoords (20, 20))

foodCoord :: GameState -> Coord
foodCoord = views foodLocations head

addSnake :: SnakeId -> GameState -> GameState
addSnake sid =
  snakes %~ M.insert sid (Snake.fromList sid [(10, 11), (10, 10), (10, 9)])

removeSnake :: SnakeId -> GameState -> GameState
removeSnake sid = snakes %~ M.delete sid

allSnakesCoords :: Traversal' GameState Coord
allSnakesCoords = snakes . traverse . Snake.pieces . traverse

step :: GameState -> GameState
step = advanceSnakes . checkFood . removeColliding

intendTurn :: SnakeId -> Direction -> GameState -> GameState
intendTurn snakeId newDirection gs =
  gs & snakes . ix snakeId %~ Snake.intendTurn newDirection

checkFood :: GameState -> GameState
checkFood gs | null eatingSnakes = gs
             | otherwise         = gs' & foodLocations %~ tail
 where
  food = foodCoord gs
  -- SO COOL
  -- With lenses you can return both the updated structure as well as the
  -- modified value
  (eatingSnakes, gs') =
    gs
      &   snakes
      .   partsOf (traverse . filtered (isEating food))
      <%~ fmap Snake.eat

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
