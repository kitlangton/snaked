module Snaked.SnakeSpec
  ( spec
  )
where

import           Test.Hspec
import           Snaked.Snake
import           Snaked.Grid
import           Control.Lens

spec :: Spec
spec = do
  describe ".advance" $ do

    it "moves the snake in its current direction" $ do
      let snake  = fromList 0 [(0, 1), (0, 0), (-1, 0)]
          snake' = fromList 0 [(0, 2), (0, 1), (0, 0)]
      advance (10, 10) snake `shouldBe` snake'


  describe ".colliding" $ do

    describe "when colliding" $ do
      it "returns True" $ do
        let snake1 = fromList 0 [(0, 1), (0, 0), (-1, 0)]
            snake2 = fromList 1 [(1, 1), (0, 1), (-1, 1)]
        colliding snake1 snake2 `shouldBe` True

    describe "when with own body" $ do
      it "returns True" $ do
        let snake1 = advance (10, 10) $ set direction N $ fromList
              0
              [(0, 0), (1, 0), (1, 1), (0, 1), (-1, 1), (-2, 1)]
        colliding snake1 snake1 `shouldBe` True

    describe "when not colliding" $ do
      it "returns False" $ do
        let snake1 = fromList 0 [(0, 1), (0, 0), (-1, 0)]
            snake2 = fromList 1 [(3, 3), (3, 4), (3, 5)]
        colliding snake1 snake2 `shouldBe` False
