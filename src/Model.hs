{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model where

import Database.Persist
import Database.Persist.TH
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
	closedAt UTCTime
	createdAt UTCTime
	updatedAt UTCTime
	dueOn UTCTime
	creator GithubUserId
Issue
	milestoneId MilestoneId
	creator GithubUserId
	assignee GithubUserId
	open Bool
  isPullRequest Bool
	title String
	body String
	locked Bool
	closedAt UTCTime
	createdAt UTCTime
	updatedAt UTCTime
IssueLabel
	issue IssueId
	label String
	color String
IssueComment
	issue IssueId
	creator GithubUserId
	body String
	createdAt UTCTime
	updatedAt UTCTime
|]
