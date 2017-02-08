{-# LANGUAGE OverloadedStrings #-}

import Database.Persist
import Database.Persist.Sqlite

import Model

main :: IO ()
main = runSqlite ":memory:" $ runMigration migrateAll
