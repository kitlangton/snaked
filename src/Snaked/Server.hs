{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snaked.Server where

import qualified Network.WebSockets            as WS

import           Snaked.UI                      ( playGame )
import           Snaked.GameState               ( GameState )
import           Snaked.Grid                    ( Direction(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

import           Text.Printf

import           Data.Aeson
import qualified Snaked.GameState              as GameState
import           Snaked.Snake

import           Control.Monad.Reader
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import           Control.Concurrent             ( threadDelay
                                                , forkIO
                                                )

data User = User {
  id :: Int,
  conn :: WS.Connection
}

data Env = Env
  { envUsers :: TVar [User]
  , envGame :: TVar GameState
  }

type ServerT = ReaderT Env IO

modify :: ([User] -> [User]) -> ServerT ()
modify f = do
  env <- ask
  liftIO $ atomically $ modifyTVar' (envUsers env) f

addUser :: WS.Connection -> ServerT ()
addUser conn = do
  users' <- asks envUsers
  user   <- liftIO $ atomically $ do
    users <- readTVar users'
    let user = User (length users) conn
    modifyTVar' users' (user :)
    return user
  forever (control user)

control :: User -> ServerT ()
control (User uid conn) = do
  Just (direction :: Direction) <- liftIO $ decode <$> WS.receiveData conn
  liftIO $ putStrLn $ "Received: " ++ show direction
  gameVar <- asks envGame
  liftIO $ atomically $ modifyTVar'
    gameVar
    (GameState.intendTurn (SnakeId uid) direction)

sendGameState :: GameState -> User -> IO ()
sendGameState gameState (User _ conn) = WS.sendTextData conn (encode gameState)

gameloop :: ServerT ()
gameloop = do
  Env usersVar gameVar <- ask

  (gameState, users)   <- liftIO $ atomically $ do
    gameState <- readTVar gameVar
    writeTVar gameVar $ GameState.step gameState
    users <- readTVar usersVar
    return (gameState, users)

  liftIO $ do
    mapM_ (sendGameState gameState) users
    threadDelay 100000
    putStrLn "tick"

  gameloop

server :: IO ()
server = do
  envGame  <- newTVarIO GameState.empty
  envUsers <- newTVarIO []
  let env = Env { .. }
  forkIO $ runReaderT gameloop env
  WS.runServer "127.0.0.1" 9160 $ handleConnection env

handleConnection env pending = do
  conn <- WS.acceptRequest pending
  runReaderT (addUser conn) env

-- CLIENT

--------------------------------------------------------------------------------
clientApp :: WS.ClientApp ()
clientApp conn = do
  putStrLn "Connected!"

  void $ playGame conn
  -- -- Fork a thread that writes WS data to stdout
  -- _ <- forkIO $ forever $ do
  --   msg <- WS.receiveData conn
  --   liftIO $ T.putStrLn msg
  --
  -- -- Read from stdin and write to WS
  -- let loop = do
  --       line <- T.getLine
  --       unless (T.null line) $ WS.sendTextData conn line >> loop
  --
  -- loop
  -- WS.sendClose conn ("Bye!" :: Text)

client = WS.runClient "127.0.0.1" 9160 "/" clientApp
