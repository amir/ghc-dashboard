{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model where

import Database.Persist
import Database.Persist.TH
import Data.Text
import Data.Time

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
GithubUser
  login String
  deriving Show
Milestone
  number Int
  open Bool
  title String
  description String
  openIssues Int
  closedIusses Int
  repository String
  createdAt UTCTime
  updatedAt UTCTime
  dueOn UTCTime Maybe
  closedAt UTCTime Maybe
  creator GithubUserId
Issue
  number Int
  milestoneId MilestoneId
  creator GithubUserId
  assignee GithubUserId
  open Bool
  isPullRequest Bool
  title String
  labels [String]
  body String
  locked Bool
  repository String
  closedAt UTCTime Maybe
  createdAt UTCTime
  updatedAt UTCTime
IssueComment
  issue IssueId
  creator GithubUserId
  body String
  repository String
  createdAt UTCTime
  updatedAt UTCTime
PullRequest
  number Int
  state String
  title String
  body String
  assignee GithubUserId
  milestone MilestoneId
  locked Bool
  commits Int
  additions Int
  deletions Int
  changedFiles Int
  repository String
  createdAt UTCTime
  updatedAt UTCTime
  closedAt UTCTime Maybe
  mergedAt UTCTime Maybe
Release
  date UTCTime
  released Bool
Build
  number Int
  builderName String
  successful Bool
  message Text
  durationSeconds Int
  startTime UTCTime Maybe
  endTime UTCTime Maybe
Team
  name String
  mention String
  label String
Membership
  member GithubUserId
  team TeamId
|]
