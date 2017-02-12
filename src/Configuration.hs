{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Configuration where

import qualified Data.Text as T
import qualified Data.ByteString as B
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, asks, lift)

data Config = Config {
    githubWebhooksSecret :: T.Text
  , githubUsername :: B.ByteString
  , githubPassword :: B.ByteString
  } deriving (Eq, Show, Read)

newtype ConfigM a = ConfigM {
  runConfigM :: ReaderT Config IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

getConfig :: IO Config
getConfig = return Config {
    githubWebhooksSecret = "secret"
  , githubUsername = "username"
  , githubPassword = "password"
  }
