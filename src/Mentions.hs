{-# LANGUAGE OverloadedStrings #-}

module Mentions where

import Data.Char (isSpace)
import Data.Either (rights)
import Data.Attoparsec.Text
import Control.Applicative

import qualified Data.Text as T

data CommandType = FCP deriving Show

data Mention = Mention {
    bot            :: T.Text
  , command        :: CommandType
  , commandParams  :: [String]
} deriving Show

type Mentions = [Mention]

commandTypeParser :: Parser CommandType
commandTypeParser = string "fcp" >> return FCP

mentionParser :: Parser Mention
mentionParser = do
  char '@'
  b <- takeTill isSpace
  skipSpace
  c <- commandTypeParser
  skipSpace
  p <- many' (letter <|> digit) `sepBy` many1 space
  return Mention { bot = b, command = c, commandParams = p }

parseIssueCommentBody   :: T.Text -> Mentions
parseIssueCommentBody c = rights $ fmap (parseOnly mentionParser) (T.lines c)
