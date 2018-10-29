{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Snaked.Snake where

import           Data.List                      ( nub )
import           Data.Maybe
import           Control.Lens.TH
import           Control.Lens
import           Control.Lens.Unsound
import           Control.Applicative
import           Data.Traversable
import           Data.Aeson

import           Snaked.Grid                    ( Direction(..)
                                                , Coord(..)
                                                , Size
                                                , move
                                                , mkCoord
                                                , deriveDirection
                                                , wrapCoord
                                                , isConnected
                                                )

newtype SnakeId = SnakeId Int
  deriving (Show, Eq, Ord, Num, Integral, Enum, Real, FromJSON, ToJSON, ToJSONKey, FromJSONKey)

data Snake = Snake {
  _snakeId :: SnakeId,
  _pieces :: [Coord],
  _hasEaten :: Bool,
  _nextDirection :: Maybe Direction,
  _direction :: Direction
} deriving (Show, Eq)

$(makeLenses ''Snake)
fromList :: SnakeId -> [(Int, Int)] -> Snake
fromList snakeId (fmap (uncurry mkCoord) -> coords)
  | length coords < 2 = error "Must call fromList with at least 2 coordinates"
  | not $ isConnected coords = error
    "Each coord must be 1 distance away from the last"
  | length coords /= length (nub coords) = error "Each coord must be unique"
  | otherwise = Snake snakeId coords False Nothing $ deriveDirection coords

eat = hasEaten .~ True

advance :: Size -> Snake -> Snake
advance size snake | snake ^. hasEaten = grow size snake & hasEaten .~ False
                   | otherwise         = advance' size snake

advance' :: Size -> Snake -> Snake
advance' size = do
  newHead <- nextHead size
  pieces %~ cons newHead . init

grow :: Size -> Snake -> Snake
grow size = do
  newHead <- nextHead size
  pieces %~ (newHead :)

nextHead :: Size -> Snake -> Coord
nextHead size Snake {..} = wrapCoord size $ move _direction (head _pieces)

snakeHead :: Snake -> Coord
snakeHead = views pieces head

finalizeTurn :: Snake -> Snake
finalizeTurn s = fromMaybe s $ do
  next <- s ^. nextDirection
  return (s & direction .~ next)

intendTurn :: Direction -> Snake -> Snake
intendTurn intendedTurn snake = if validTurn intendedTurn (snake ^. direction)
  then snake & nextDirection ?~ intendedTurn
  else snake

data Axis = Horizontal | Vertical deriving (Show, Eq)

axis (flip elem [N,S] -> True) = Vertical
axis (flip elem [E,W] -> True) = Horizontal

validTurn :: Direction -> Direction -> Bool
validTurn next current = axis next /= axis current

snakeTail :: Snake -> [Coord]
snakeTail = views pieces tail

isEating :: Coord -> Snake -> Bool
isEating foodCoord s = snakeHead s == foodCoord

colliding :: Snake -> Snake -> Bool
colliding s1 s2 | (s1 ^. snakeId) == (s2 ^. snakeId) = selfColliding s1
                | otherwise = snakeHead s1 `elem` s2 ^. pieces

selfColliding :: Snake -> Bool
selfColliding = liftA2 elem snakeHead snakeTail
