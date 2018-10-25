{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Snaked where

import           Snaked.Grid                    ( Size
                                                , Coord
                                                , Direction
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

data SnakeState = SnakeState {
  _snakes :: SnakeMap,
  _size :: Size
} deriving Show

type SnakeT a = State SnakeState a

$(makeLenses ''SnakeState)

run snaket = execState snaket defaultSnakeState
example n = execState (replicateM_ n step) defaultSnakeState

defaultSnakeState = SnakeState
  (M.fromList
    [ (SnakeId 0, Snake.fromList 0 [(2, 2), (1, 2)])
    , (SnakeId 1, Snake.fromList 1 $ reverse [(4, 4), (3, 4), (2, 4), (2, 5)])
    ]
  )
  (20, 20)

allSnakesCoords :: Traversal' SnakeState Coord
allSnakesCoords = snakes . traverse . Snake.pieces . traverse

step :: SnakeT ()
step = advanceSnakes >> removeColliding

turn :: SnakeId -> Direction -> SnakeT ()
turn snakeId newDirection =
  snakes . ix snakeId . Snake.direction .= newDirection

-- Advances snakes in their current directions
advanceSnakes :: SnakeT ()
advanceSnakes = do
  size <- use size
  snakes . traverse %= Snake.advance size

-- Removes colliding snakes
removeColliding :: SnakeT ()
removeColliding = snakes %= removeColliding'

removeColliding' :: SnakeMap -> SnakeMap
removeColliding' snakeMap =
  M.filter (\s -> not $ any (Snake.colliding s) snakeMap) snakeMap
