{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Data.Aeson hiding (json)
import GHC.Generics
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import GitHub.Types

readEvent :: TL.Text -> Maybe GithubEventType
readEvent e = readMaybe (TL.unpack $ TL.toTitle e)

act :: Object -> GithubEventType -> Maybe TL.Text
act payload event = Just $ TL.pack (show event)

app :: ScottyM ()
app = 
  post "/github-webhooks" $ do
    b  <- jsonData
    e  <- header "X-Github-Event"
    text $ fromMaybe (TL.pack "") ((e >>= readEvent) >>= act b)

main :: IO ()
main = scotty 8080 app
