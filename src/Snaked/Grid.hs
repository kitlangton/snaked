{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Snaked.Grid where

import           Linear.V2
import           Data.Bifunctor
import           Control.Arrow
import           Control.Lens
import qualified Network.WebSockets            as WS
import           Data.Text                      ( Text )

import           Data.Aeson.TH

data Direction = N | E | S | W deriving (Show, Enum, Eq, Ord)

type Coord = V2 Int
type Size = (Int, Int)

mkCoord :: Int -> Int -> Coord
mkCoord = V2

delta :: Direction -> Coord
delta N = V2 0 1
delta S = V2 0 (-1)
delta E = V2 1 0
delta W = V2 (-1) 0

move :: Direction -> Coord -> Coord
move = (+) . delta

deriveDirection :: [Coord] -> Direction
deriveDirection (a : b : _) = go a b
 where
  go (V2 x1 y1) (V2 x2 y2) | x1 > x2 = E
                           | x1 < x2 = W
                           | y1 > y2 = N
                           | y1 < y2 = S

wrapCoord :: Size -> Coord -> Coord
wrapCoord (width, height) = _x %~ wrap width <<< _y %~ wrap height
 where
  wrap :: Int -> Int -> Int
  wrap top current | current < 0    = top + current
                   | current >= top = current `mod` top
                   | otherwise      = current

isConnected :: [Coord] -> Bool
isConnected cs = and $ zipWith connected cs (tail cs)
 where
  connected :: Coord -> Coord -> Bool
  connected c1 c2 = dist c1 c2 == 1

  dist :: Coord -> Coord -> Int
  dist c1 c2 = abs $ mdist c1 - mdist c2

  mdist :: Coord -> Int
  mdist = sum . fmap abs
