{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.Aeson.Types
import Data.Monoid ((<>))
import GitHub.Auth
import GitHub.Data.Webhooks
import GitHub.Data.Webhooks.Validate
import Data.Default.Class (def)
import Network.HTTP.Types.Status
import Control.Monad.Reader (runReaderT, asks, lift)
import Web.Scotty.Trans (ScottyT, scottyOptsT, post, header, body, status)

import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as B
import qualified Data.Text.Lazy.Encoding as E

import Configuration
import Webhooks

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
      Nothing     -> status badRequest400

    else status badRequest400

  where
    event :: Maybe TL.Text -> Maybe RepoWebhookEvent
    event z = fmap E.encodeUtf8 z >>= \x ->
      decode $ B.toLazyByteString $ q <> B.lazyByteString x <> q
    q = B.charUtf8 '\"'
    isValid se si bo = isValidPayload se (fmap TL.toStrict si) (BL.toStrict bo)

application :: Config -> ScottyT Error ConfigM ()
application c =
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
