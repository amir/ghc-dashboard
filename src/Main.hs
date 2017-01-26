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
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Crypto.Hash as Hash

import GitHub.Types

readEvent :: TL.Text -> Maybe GithubEventType
readEvent e = readMaybe (TL.unpack $ TL.toTitle e)

act :: Object -> GithubEventType -> Maybe TL.Text
act payload event = Just $ TL.pack (show event)

checkSignature :: TL.Text -> BL.ByteString -> Bool
checkSignature signature body =
  signature == digest
  where
    digest = TL.pack $ show $ sha1 $ BL.toStrict body

    sha1 :: B.ByteString -> Hash.Digest Hash.SHA1
    sha1  = Hash.hash

app :: ScottyM ()
app = do
  post "/github-webhooks" $ do
    b  <- body
    s  <- header "X-Hub-Signature"
    text $ TL.pack $ show $ checkSignature (fromMaybe TL.empty s) b

main :: IO ()
main = scotty 8080 app
