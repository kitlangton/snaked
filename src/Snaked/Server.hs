{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Snaked.Server where

import           Control.Concurrent             ( forkIO
                                                , threadDelay
                                                )
import           Control.Exception.Lifted       ( finally )
import           Control.Lens
import           Control.Lens.TH
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Aeson
import           Data.IORef
import           Data.Maybe                     ( fromMaybe )
import           Data.Tuple
import qualified Network.WebSockets            as WS
import           Snaked.GameState               ( GameState )
import qualified Snaked.GameState              as GameState
import           Snaked.Snake
import           Snaked.UI                      ( playGame )
import           Text.Printf

data User = User
  { _id   :: Int
  , _conn :: WS.Connection
  }

data Env = Env
  { _envUsers :: [User]
  , _envGame  :: GameState
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
  finally (forever $ control user) (atomically $ removeUser' user)

removeUser' :: User -> State Env ()
removeUser' (User uid _) = do
  envUsers %= filter ((/= uid) . _id)
  envGame %= GameState.removeSnake (SnakeId uid)

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
