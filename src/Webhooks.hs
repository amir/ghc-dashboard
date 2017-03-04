{-# LANGUAGE OverloadedStrings #-}

module Webhooks where

import Data.Aeson
import Data.Aeson.Types
import GitHub.Auth
import GitHub.Data.Issues
import GitHub.Data.Webhooks
import Network.HTTP.Types.Status
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe, listToMaybe)
import Web.Scotty.Trans (ActionT, status, text)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL

import qualified GitHub.Data as Github
import qualified GitHub.Data.Id as Github

import Mentions
import Configuration

type Error = TL.Text
type Action = ActionT Error ConfigM()

act :: BL.ByteString -> GitHub.Auth.Auth -> RepoWebhookEvent -> Action
act body auth WebhookIssueCommentEvent =
  case action of
    Just a -> do
      actionResult <- liftIO a
      case actionResult of
        Right r -> text $ TL.pack $ show r
        Left  e -> text $ TL.pack $ show e

    Nothing -> status badRequest400

  where
    action   = actOnMention auth <$> owner <*> repo <*> issueId <*> listToMaybe (fromMaybe [] mentions)
    mentions = fmap (parseIssueCommentBody . issueCommentBody) comment

    extract field = do
      d <- decode body
      flip parseMaybe d $ flip (.:) field

    issue :: Maybe Issue
    issue = extract "issue"

    comment :: Maybe IssueComment
    comment = extract "comment"

    owner :: Maybe (Github.Name Github.Owner)
    owner = fmap Github.mkOwnerName (part 4)

    repo :: Maybe (Github.Name Github.Repo)
    repo = fmap Github.mkRepoName (part 5)

    issueId :: Maybe (Github.Id Issue)
    issueId = fmap ((Github.Id . read) . T.unpack) (part 7)

    parts = fmap ((T.splitOn "/" . Github.getUrl) . Github.issueUrl) issue
    part :: Int -> Maybe T.Text
    part n | n < length ps = Just $ ps !! n
           | otherwise     = Nothing
      where ps = fromMaybe [] parts

act _    event _ = text $ TL.pack $ show event
