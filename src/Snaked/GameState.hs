{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Snaked.GameState where

import           Snaked.Grid                    ( Size
                                                , Coord
                                                , Direction(..)
                                                )
import           Control.Lens.TH
import           Control.Lens
import           Control.Monad.State
import qualified Data.Map.Strict               as M

import           Snaked.Snake                   ( Snake
                                                , SnakeId(..)
                                                )
import qualified Snaked.Snake                  as Snake

type SnakeMap = M.Map SnakeId Snake

data GameState = GameState {
  _snakes :: SnakeMap,
  _size :: Size
} deriving Show

type SnakeT a = State GameState a

$(makeLenses ''GameState)

defaultGameState = GameState
  (M.fromList
    [ (SnakeId 0, Snake.fromList 0 [(2, 2), (1, 2)])
    , (SnakeId 1, Snake.fromList 1 $ reverse [(4, 4), (3, 4), (2, 4), (2, 5)])
    ]
  )
  (20, 20)

allSnakesCoords :: Traversal' GameState Coord
allSnakesCoords = snakes . traverse . Snake.pieces . traverse

step :: SnakeT ()
step = advanceSnakes >> removeColliding

intendTurn :: SnakeId -> Direction -> SnakeT ()
intendTurn snakeId newDirection = snakes . ix snakeId %= Snake.intendTurn newDirection

-- Advances snakes in their current directions
advanceSnakes :: SnakeT ()
advanceSnakes = do
  size <- use size
  snakes . traverse %= (Snake.advance size . Snake.finalizeTurn)

-- Removes colliding snakes
removeColliding :: SnakeT ()
removeColliding = snakes %= removeColliding'

removeColliding' :: SnakeMap -> SnakeMap
removeColliding' snakeMap =
  M.filter (\s -> not $ any (Snake.colliding s) snakeMap) snakeMap