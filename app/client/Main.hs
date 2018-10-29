module Main where

import           Snaked.Server
import           System.Environment
import           Data.Maybe

safeHead :: [a] -> Maybe a
safeHead (a : _) = Just a
safeHead _       = Nothing

main :: IO ()
main = do
  serverUrl <- fromMaybe "127.0.0.1" . safeHead <$> getArgs
  client serverUrl
