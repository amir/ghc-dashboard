{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)

import Database.Persist as DB
import Database.Persist.Sqlite as DB
import Data.Vector

import qualified Data.Text as T
import qualified GitHub.Auth as Github
import qualified GitHub.Request as Github
import qualified GitHub.Endpoints.Organizations.Teams as Github
import qualified GitHub.Endpoints.Organizations.Members as Github

import Model
import Configuration

auth :: Config -> Github.Auth
auth c = Github.BasicAuth (githubUsername c) (githubPassword c)

migrateSchema :: Config -> IO ()
migrateSchema c =
  liftIO $ flip DB.runSqlPersistMPool (pool c) $ DB.runMigration migrateAll

runQuery :: Config -> DB.SqlPersistT IO a -> IO a
runQuery c q = DB.runSqlPool q (pool c)

teamMembers :: Github.Auth -> Github.Id Github.Team -> IO (Either Github.Error (Vector Github.SimpleUser))
teamMembers a team = Github.executeRequest a $
  Github.listTeamMembersR team Github.TeamMemberRoleAll Github.FetchAll

getUserKey :: PersistEntity r => Github.SimpleUser -> Key r
getUserKey u = case DB.keyFromValues [DB.PersistInt64 (fromIntegral $ Github.untagId $ Github.simpleUserId u)] of
  Right uid -> uid
  Left  a   -> error $ T.unpack a

getTeamKey :: PersistEntity r => Github.SimpleTeam -> Key r
getTeamKey t = case DB.keyFromValues [DB.PersistInt64 (fromIntegral $ Github.untagId $ Github.simpleTeamId t)] of
  Right tid -> tid
  Left a    -> error $ T.unpack a

main :: IO ()
main = do
  c       <- getConfig
  migrateSchema c
  members <- Github.membersOf' (Just $ auth c) "amirghc"
  case members of
    Right ms ->
      forM_ ms $ \m ->
        runQuery c (DB.repsert (getUserKey m) (GithubUser (Github.untagName (Github.simpleUserLogin m))))

  teams <- Github.teamsOf' (Just $ auth c) "amirghc"
  case teams of
    Right ts ->
      forM_ ts $ \t -> do
        runQuery c (DB.repsert (getTeamKey t)
          (Team (Github.simpleTeamName t) (Github.untagName (Github.simpleTeamSlug t))))

        members <- teamMembers (auth c) (Github.simpleTeamId t)
        case members of
          Right ms ->
            forM_ ms $ \m ->
              runQuery c (DB.insertUnique (Membership (getUserKey m) (getTeamKey t)))
