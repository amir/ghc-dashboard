{-# LANGUAGE OverloadedStrings #-}

module Mentions where

import Data.Char (isSpace)
import Data.Either (rights)
import Data.Attoparsec.Text
import Control.Applicative

import qualified Data.Text as T

data CommandType = CommandFCP
                   deriving Show

data CommandParamType = ParamClose
                        deriving Show

data Mention = Mention {
    bot            :: T.Text
  , command        :: CommandType
  , commandParams  :: CommandParamType
} deriving Show

type Mentions = [Mention]

commandTypeParser :: Parser CommandType
commandTypeParser = asciiCI "fcp" >> return CommandFCP

commandParamParser :: CommandType -> Parser CommandParamType
commandParamParser CommandFCP = asciiCI "close" >> return ParamClose

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
