module GitHub.Types where

data GithubEventType = Ping
                     | Issues
                     | Unknown
                     deriving (Read, Show, Eq)
