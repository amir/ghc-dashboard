{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Data.Aeson
import Data.Aeson.Types
import Data.Monoid ((<>))
import GitHub.Data.Issues
import GitHub.Data.Webhooks
import GitHub.Data.Webhooks.Validate
import Network.HTTP.Types.Status

import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as B
import qualified Data.Text.Lazy.Encoding as E

import Mentions

act :: BL.ByteString -> RepoWebhookEvent -> TL.Text
act body WebhookIssueCommentEvent =
  TL.pack $ show $ fmap parseIssueCommentBody commentBody
  where
    commentBody = fmap issueCommentBody (comment body)
    comment c = do
      result <- decode c
      flip parseMaybe result $ flip (.:) "comment"

act _    event = TL.pack $ show event

app :: ScottyM ()
app = do
  post "/github-webhooks" $ do
    b <- body
    s <- header "X-Hub-Signature"
    e <- header "X-Github-Event"
    if isValid s b
      then case event e of
        Just e' -> text $ act b e'
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
