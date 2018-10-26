{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}

module Snaked.Server where

import qualified Network.WebSockets            as WS

import           Snaked.GameState               ( GameState )
import           Snaked.Grid                    ( Direction(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T


import qualified Snaked.GameState              as GameState

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
  direction <- liftIO $ WS.receiveData conn :: ServerT Direction
  liftIO $ print direction

gameloop :: ServerT ()
gameloop = do
  gameVar <- asks envGame
  liftIO $ atomically $ modifyTVar' gameVar GameState.step
  liftIO $ threadDelay 100000
  liftIO $ putStrLn "tick"
  gameloop

server :: IO ()
server = do
  envGame  <- newTVarIO GameState.empty
  envUsers <- newTVarIO []
  let env = Env { .. }
  forkIO $ runReaderT gameloop env
  WS.runServer "127.0.0.1" 9160 $ handleConnection env

fromText :: Text -> Direction
fromText "N" = N
fromText "S" = S
fromText "E" = E
fromText "W" = W

instance WS.WebSocketsData Direction where
  fromDataMessage = fromText . WS.fromDataMessage

handleConnection env pending = do
  conn <- WS.acceptRequest pending
  runReaderT (addUser conn) env


-- CLIENT

--------------------------------------------------------------------------------
clientApp :: WS.ClientApp ()
clientApp conn = do
  putStrLn "Connected!"

  -- Fork a thread that writes WS data to stdout
  _ <- forkIO $ forever $ do
    msg <- WS.receiveData conn
    liftIO $ T.putStrLn msg

  -- Read from stdin and write to WS
  let loop = do
        line <- T.getLine
        unless (T.null line) $ WS.sendTextData conn line >> loop

  loop
  WS.sendClose conn ("Bye!" :: Text)

client = WS.runClient "127.0.0.1" 9160 "/" clientApp
