{-# LANGUAGE FlexibleContexts #-}
module Snaked.Grid where

import           Linear.V2
import           Control.Arrow
import           Control.Lens
import qualified Network.WebSockets            as WS

import           Data.Aeson.TH
import           System.Random

data Direction = N | E | S | W deriving (Show, Enum, Eq, Ord)

type Coord = V2 Int
type Size = (Int, Int)

instance Random a => Random (V2 a) where
  randomR (V2 l1 l2, V2 h1 h2) g =
    let
      (a,g') = randomR (l1,h1) g
      (b,g'') = randomR (l2,h2) g'
    in
     (V2 a b, g'')
  random g =
    let
      (a,g') = random g
      (b,g'') = random g'
    in
     (V2 a b, g'')

randomCoords :: Size -> [Coord]
randomCoords (a, b) = randomRs (V2 0 0, V2 a b) $ mkStdGen 1

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
