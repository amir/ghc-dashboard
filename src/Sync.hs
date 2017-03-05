{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)

import GitHub.Data.Definitions

import Database.Persist as DB
import Database.Persist.Sqlite as DB

import qualified GitHub.Auth as Github
import qualified GitHub.Endpoints.Organizations.Members as Github

import Model
import Configuration

auth :: Config -> Maybe Github.Auth
auth c = Just $ Github.BasicAuth (githubUsername c) (githubPassword c)

migrateSchema :: Config -> IO ()
migrateSchema c =
  liftIO $ flip DB.runSqlPersistMPool (pool c) $ DB.runMigration migrateAll

runQuery :: Config -> DB.SqlPersistT IO a -> IO a
runQuery c q = DB.runSqlPool q (pool c)

main :: IO ()
main = do
  c       <- getConfig
  migrateSchema c
  members <- Github.membersOf' (auth c) "amirghc"
  case members of
    Right ms ->
      forM_ ms $ \m -> runQuery c (DB.insert (GithubUser $ show (simpleUserLogin m)))
