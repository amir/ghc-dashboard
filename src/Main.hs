{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Data.Aeson
import Data.Aeson.Types
import Data.Monoid ((<>))
import GitHub.Auth
import GitHub.Data.Issues
import GitHub.Data.Webhooks
import GitHub.Data.Webhooks.Validate
import Network.HTTP.Types.Status
import Control.Monad.IO.Class (liftIO)

import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as B
import qualified Data.Text.Lazy.Encoding as E

import Mentions

act :: BL.ByteString -> RepoWebhookEvent -> ActionM ()
act body WebhookIssueCommentEvent =
  case action of
    Just a -> do
      actionResult <- liftIO a
      case actionResult of
        Right r -> text $ TL.pack $ show r
        Left  e -> text $ TL.pack $ show e
    Nothing ->
      status badRequest400
  where
    auth     = BasicAuth "username" "password"
    action   = fmap (actOnMention auth issue . head) mentions
    mentions = fmap (parseIssueCommentBody . issueCommentBody) comment

    extract field = do
      d <- decode body
      flip parseMaybe d $ flip (.:) field

    issue :: Maybe Issue
    issue = extract "issue"

    comment :: Maybe IssueComment
    comment = extract "comment"

act _    event = text $ TL.pack $ show event

app :: ScottyM ()
app =
  post "/github-webhooks" $ do
    b <- body
    s <- header "X-Hub-Signature"
    e <- header "X-Github-Event"
    if isValid s b
      then case event e of
        Just e' -> act b e'
        Nothing -> status badRequest400

      else status badRequest400
    where
      event :: Maybe TL.Text -> Maybe RepoWebhookEvent
      event z = fmap E.encodeUtf8 z >>= \x ->
        decode $ B.toLazyByteString $ q <> B.lazyByteString x <> q
      q = B.charUtf8 '\"'
      isValid s b = isValidPayload secret (fmap TL.toStrict s) (BL.toStrict b)
      secret = "Secret"

main :: IO ()
main = scotty 8080 app
