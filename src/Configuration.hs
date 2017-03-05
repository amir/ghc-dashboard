{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Configuration where

import qualified Data.Text as T
import qualified Data.ByteString as B
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, asks, lift)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Trans.Class (MonadTrans, lift)

import Database.Persist as DB
import Database.Persist.Sqlite as DB

data Config = Config {
    githubWebhooksSecret :: T.Text
  , githubUsername :: B.ByteString
  , githubPassword :: B.ByteString
  , pool           :: DB.ConnectionPool
  } deriving Show

newtype ConfigM a = ConfigM {
  runConfigM :: ReaderT Config IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

getPool :: IO DB.ConnectionPool
getPool = runStdoutLoggingT (DB.createSqlitePool ":memory:" 1)

runDB :: (MonadTrans t, MonadIO (t ConfigM)) => DB.SqlPersistT IO a -> t ConfigM a
runDB q = do
  p <- lift (asks pool)
  liftIO (DB.runSqlPool q p)

getConfig :: IO Config
getConfig = do
  p <- getPool
  return Config {
      pool                 = p
    , githubUsername       = "username"
    , githubPassword       = "password"
    , githubWebhooksSecret = "secret"
  }
