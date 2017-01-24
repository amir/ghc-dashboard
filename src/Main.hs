{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Data.Aeson hiding (json)
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BSL

import Data.Monoid (mconcat)

data Hook = Hook {
  name :: T.Text
} deriving (Show, Eq, Generic)

data WebhookPayload = WebhookPayload {
  hook :: Hook
} deriving (Show, Eq, Generic)

instance ToJSON Hook
instance FromJSON Hook

instance ToJSON WebhookPayload
instance FromJSON WebhookPayload

githubWebhooks :: WebhookPayload -> TL.Text
githubWebhooks g = TL.fromStrict $ name (hook g)

app :: ScottyM ()
app = 
  post "/github-webhooks" $ do
    b <- jsonData :: ActionM WebhookPayload
    text $ githubWebhooks b

main :: IO ()
main = scotty 8080 app
