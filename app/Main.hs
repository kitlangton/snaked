module Main where

import           Snaked.UI                      ( playGame )
import           Snaked.GameState               ( GameState )
import           Snaked.Server

main :: IO ()
main = server
