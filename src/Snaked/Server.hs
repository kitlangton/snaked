{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}

module Snaked.Server where

import           Control.Concurrent             ( forkIO
                                                , threadDelay
                                                )
import           Control.Exception.Lifted
import           Control.Lens
import           Control.Lens.TH
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Control
import           Data.Aeson
import           Data.IORef
import           Control.Monad.Base
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe )
import           Data.Tuple
import qualified Network.WebSockets            as WS
import           Snaked.GameState               ( GameState )
import qualified Snaked.GameState              as GameState
import           Snaked.Grid                    ( Direction )
import           Snaked.UI                      ( playGame )
import           Text.Printf

data User = User
  { _id   :: Int
  , _conn :: WS.Connection
  }

instance Show User where
  show (User uid _) = "User " ++ show uid

data Env = Env
  { _envUsers :: M.Map Int User
  , _envGame  :: GameState
  }

$(makeLenses ''Env)

newtype Server a = Server
  { runServer :: ReaderT (IORef Env) IO a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader (IORef Env)
             , MonadIO
             , MonadBase IO
             , MonadBaseControl IO
             )

-- Server API
  -- Must broadcast old gamestate for some order-of-operations reason I don't
  -- fully understand at the moment.
gameloop :: Server ()
gameloop = forever $ do
  (gameState, users) <- atomically $ do
    users <- use envUsers
    game  <- envGame <%= GameState.step
    return (game, users)
  liftIO $ forkIO $ mapM_ (sendGameState gameState) users
  liftIO $ threadDelay 100000

addUser :: WS.Connection -> Server ()
addUser conn = do
  user <- atomically (addUser' conn)
  finally (handleUserEvents user) (atomically $ removeUser user)

handleUserEvents :: User -> Server ()
handleUserEvents user@(User uid conn) = forever $ do
  Just newDirection <- liftIO $ decode <$> WS.receiveData conn
  liftIO $ putStrLn $ printf "User %d -> %s" uid (show newDirection)
  atomically $ intendTurn user newDirection

-- Server Helpers
atomically :: State Env a -> Server a
atomically f = do
  envRef <- ask
  liftIO $ atomicModifyIORef' envRef (swap . runState f)

sendGameState :: GameState -> User -> IO ()
sendGameState gameState (User _ conn) =
  WS.sendTextData conn (encode $ GameState.renderableState gameState)

-- Env Functions
intendTurn :: User -> Direction -> State Env ()
intendTurn (User uid _) newDirection =
  modify $ envGame %~ GameState.intendTurn (fromIntegral uid) newDirection

addUser' :: WS.Connection -> State Env User
addUser' conn = do
  users <- use envUsers
  let userId = maybe 0 ((+ 1) . fst) (M.lookupMax users)
      user   = User userId conn
  envGame %= GameState.addSnake (fromIntegral userId)
  envUsers %= M.insert userId user
  return user

removeUser :: User -> State Env ()
removeUser (User uid _) = do
  envUsers %= M.delete uid
  envGame %= GameState.removeSnake (fromIntegral uid)

-- SERVER
server :: IO ()
server = do
  envVar <- newIORef (Env M.empty GameState.empty)
  _      <- forkIO $ runReaderT (runServer gameloop) envVar
  WS.runServer "127.0.0.1" 9160 $ handleConnection envVar

handleConnection :: IORef Env -> WS.PendingConnection -> IO ()
handleConnection env pending = do
  conn <- WS.acceptRequest pending
  runReaderT (runServer (addUser conn)) env

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
