{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StrictData #-}
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
import           Control.Exception              ( finally )
import           Control.Concurrent             ( threadDelay
                                                , forkIO
                                                )
import           Control.Lens.TH
import           Control.Monad.State
import           Control.Lens
import           Data.IORef
import           Data.Tuple
import qualified Data.Map.Strict as M

data User = User {
  _id :: Int,
  _conn :: WS.Connection
}

data Env = Env
  { _envUsers :: M.Map Int User
  , _envGame :: GameState
  }

$(makeLenses ''Env)

type Server = ReaderT (IORef Env) IO

-- Server API

gameloop :: Server ()
gameloop = forever $ do
  -- Must broadcast old gamestate for some order-of-operations reason I don't
  -- fully understand at the moment.
  (gameState, users) <- atomically $ do
    oldGame <- use envGame
    users   <- use envUsers
    envGame %= GameState.step
    return (oldGame, users)

  liftIO $ do
     mapM_ (sendGameState gameState) users
     threadDelay 100000

addUser :: WS.Connection -> Server ()
addUser conn = do
  user <- atomically (addUser' conn)
  finallyReaderT (handleUserEvents user) (atomically $ removeUser user)

handleUserEvents :: User -> Server ()
handleUserEvents (User uid conn) = forever $ do
  Just newDirection <- liftIO $ decode <$> WS.receiveData conn
  liftIO $ putStrLn $ printf "User %d -> %s" uid (show newDirection)
  atomically $ modify $ envGame %~ GameState.intendTurn (SnakeId uid) newDirection

-- Server Helpers

atomically :: State Env a -> Server a
atomically f = do
  envRef <- ask
  liftIO $ atomicModifyIORef' envRef (swap . runState f)

sendGameState :: GameState -> User -> IO ()
sendGameState gameState (User _ conn) = WS.sendTextData conn (encode gameState)

-- Env Functions

addUser' :: WS.Connection -> State Env User
addUser' conn = do
  users <- use envUsers
  let userId = maybe 0 fst (M.lookupMax users)
      user   = User userId conn
  envGame %= GameState.addSnake (SnakeId userId)
  envUsers %= M.insert userId user
  return user

removeUser :: User -> State Env ()
removeUser (User uid _) = do
  envUsers %= M.delete uid
  envGame %= GameState.removeSnake (SnakeId uid)

-- SERVER

server :: IO ()
server = do
  envVar <- newIORef (Env M.empty GameState.empty)
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

-- BONUS

finallyReaderT :: ReaderT r IO a -> ReaderT r IO b -> ReaderT r IO a
finallyReaderT a sequel = do
  r <- ask
  liftIO $ runReaderT a r `finally` runReaderT sequel r
