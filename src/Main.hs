{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty.Trans (ActionT, ScottyT, scottyOptsT, post, json, text, header, body, status)
import Data.Default.Class (def)
import Data.Aeson
import Data.Aeson.Types hiding (Options)
import Data.Monoid ((<>))
import GitHub.Auth
import GitHub.Data.Issues
import GitHub.Data.Webhooks
import GitHub.Data.Webhooks.Validate
import Network.HTTP.Types.Status
import Control.Monad.Reader (Reader, runReaderT, runReader, asks, lift)
import Control.Monad.IO.Class (liftIO)

import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as B
import qualified Data.Text.Lazy.Encoding as E

import Mentions
import Configuration

act :: BL.ByteString -> GitHub.Auth.Auth -> RepoWebhookEvent -> Action
act body auth WebhookIssueCommentEvent =
  case action of
    Just a -> do
      actionResult <- liftIO a
      case actionResult of
        Right r -> text $ TL.pack $ show r
        Left  e -> text $ TL.pack $ show e
    Nothing ->
      status badRequest400
  where
    action   = fmap (actOnMention auth issue . head) mentions
    mentions = fmap (parseIssueCommentBody . issueCommentBody) comment

    extract field = do
      d <- decode body
      flip parseMaybe d $ flip (.:) field

    issue :: Maybe Issue
    issue = extract "issue"

    comment :: Maybe IssueComment
    comment = extract "comment"


act _    event _ = text $ TL.pack $ show event

type Error = TL.Text

type Action = ActionT Error ConfigM ()

githubWebhooksA :: Action
githubWebhooksA = do
  b        <- body
  s        <- header "X-Hub-Signature"
  e        <- header "X-Github-Event"
  secret   <- lift $ asks githubWebhooksSecret
  username <- lift $ asks githubUsername
  password <- lift $ asks githubPassword
  if isValid secret s b
    then case event e of
      Just event' -> act b (BasicAuth username password) event'
      Nothing -> status badRequest400

    else status badRequest400

  where
    event :: Maybe TL.Text -> Maybe RepoWebhookEvent
    event z = fmap E.encodeUtf8 z >>= \x ->
      decode $ B.toLazyByteString $ q <> B.lazyByteString x <> q
    q = B.charUtf8 '\"'
    isValid se si bo = isValidPayload se (fmap TL.toStrict si) (BL.toStrict bo)

application :: Config -> ScottyT Error ConfigM ()
application c = do
  post "/github-webhooks" githubWebhooksA

runApplication :: Config -> IO ()
runApplication c = do
  let r m = runReaderT (runConfigM m) c
      app = application c
  scottyOptsT def r app

main :: IO ()
main = do
  c <- getConfig
  runApplication c
