{-# LANGUAGE OverloadedStrings #-}

module Mentions where

import Data.Char (isSpace)
import Data.Either (rights)
import Data.Attoparsec.Text
import Control.Applicative
import GitHub.Data (mkOwnerName, mkRepoName)

import qualified Data.Text as T

import qualified GitHub.Auth as Github
import qualified GitHub.Data.Issues as Github
import qualified GitHub.Data.URL as Github
import qualified GitHub.Endpoints.Issues as Github
import qualified GitHub.Endpoints.Issues.Comments as Github
import qualified GitHub.Data.Id as Github
import qualified GitHub.Data.Options as Github
import qualified GitHub.Data as Github

data CommandType = CommandFCP
                   deriving Show

data CommandParamType = ParamClose
                      | ParamMerge
                      deriving Show

data Mention = Mention {
    bot            :: T.Text
  , command        :: CommandType
  , commandParams  :: CommandParamType
} deriving Show

type Mentions = [Mention]

actOnMention :: Github.Auth -> Github.Name Github.Owner -> Github.Name Github.Repo -> Github.Id Github.Issue -> Mention ->
  IO (Either Github.Error Github.Issue)
actOnMention auth owner repo giid (Mention _ CommandFCP ParamClose) =
  Github.editIssue auth owner repo giid edit
  where
    edit = Github.editOfIssue { Github.editIssueState = Just Github.StateClosed }

actOnMention auth owner repo giid (Mention _ CommandFCP ParamMerge) = do
  _ <- Github.createComment auth owner repo giid edit
  Github.issue owner repo giid
  where
    edit = "no merge for you today"

commandTypeParser :: Parser CommandType
commandTypeParser = asciiCI "fcp" >> return CommandFCP

commandParamParser :: CommandType -> Parser CommandParamType
commandParamParser CommandFCP =
     (asciiCI "close" >> return ParamClose)
 <|> (asciiCI "merge" >> return ParamMerge)

mentionParser :: Parser Mention
mentionParser = do
  char '@'
  b <- takeTill isSpace
  skipSpace
  c <- commandTypeParser
  skipSpace
  p <- commandParamParser c
  skipSpace <* endOfInput
  return Mention { bot = b, command = c, commandParams = p }

parseIssueCommentBody   :: T.Text -> Mentions
parseIssueCommentBody c = rights $ fmap (parseOnly mentionParser) (T.lines c)
