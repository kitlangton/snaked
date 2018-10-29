{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snaked.Server where

import qualified Network.WebSockets            as WS
import           Snaked.UI                      ( playGame )
import           Snaked.GameState               ( GameState )
import           Text.Printf
import           Data.Aeson
import           Data.Maybe                     ( fromMaybe )
import qualified Snaked.GameState              as GameState
import           Snaked.Snake
import           Control.Monad.Reader
import           Control.Concurrent             ( threadDelay
                                                , forkIO
                                                )
import           Control.Lens.TH
import           Control.Monad.State
import           Control.Lens
import           Data.IORef
import           Data.Tuple

data User = User {
  id :: Int,
  conn :: WS.Connection
}

data Env = Env
  { _envUsers :: [User]
  , _envGame :: GameState
  }

$(makeLenses ''Env)

type Server = ReaderT (IORef Env) IO

addUser' :: WS.Connection -> State Env User
addUser' conn = do
  users <- use envUsers
  let userId = length users
      user   = User userId conn
  envGame %= GameState.addSnake (SnakeId userId)
  envUsers %= (user :)
  return user

addUser :: WS.Connection -> Server ()
addUser conn = do
  user <- atomically (addUser' conn)
  forever (control user)

atomically :: State Env a -> Server a
atomically f = do
  envRef <- ask
  liftIO $ atomicModifyIORef' envRef (swap . runState f)

control :: User -> Server ()
control (User uid conn) = do
  Just dir <- liftIO $ decode <$> WS.receiveData conn
  liftIO $ putStrLn $ printf "User %d - %s" uid (show dir)
  atomically $ modify (envGame %~ GameState.intendTurn (SnakeId uid) dir)

sendGameState :: GameState -> User -> IO ()
sendGameState gameState (User _ conn) = WS.sendTextData conn (encode gameState)

gameloop :: Server ()
gameloop = do
  (gameState, users) <- atomically $ do
    oldGame <- use envGame
    users   <- use envUsers
    envGame %= GameState.step
    return (oldGame, users)

  liftIO $ mapM_ (sendGameState gameState) users

  liftIO $ threadDelay 100000
  gameloop

server :: IO ()
server = do
  envVar <- newIORef (Env [] GameState.empty)
  _      <- forkIO $ runReaderT gameloop envVar
  WS.runServer "127.0.0.1" 9160 $ handleConnection envVar

handleConnection :: IORef Env -> WS.PendingConnection -> IO ()
handleConnection env pending = do
  conn <- WS.acceptRequest pending
  runReaderT (addUser conn) env

-- CLIENT

clientApp :: WS.ClientApp ()
clientApp conn = void $ playGame conn

client :: String -> IO ()
client serverUrl = WS.runClient serverUrl 9160 "/" clientApp
