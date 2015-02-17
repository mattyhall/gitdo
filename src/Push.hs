{-# LANGUAGE OverloadedStrings #-}
module Push where

import Control.Applicative
import Network.Wreq
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Database.SQLite.Simple
import Turtle
import Shared

data Config = Config { _issueUser :: T.Text
                     , _issuePswd :: T.Text
                     , _user      :: T.Text
                     , _repo      :: T.Text
                     } deriving (Show, Eq)

type ConfigM = ReaderT (Connection, Config) IO

cfgPath :: IsString a => a
cfgPath = ".git/hooks/gitdo.json"

createIssue :: Todo -> ConfigM ()
createIssue (Todo fp ln td _) = undefined

updateIssue :: Todo -> ConfigM ()
updateIssue = undefined

createOrUpdate :: Todo -> ConfigM ()
createOrUpdate t@(Todo _ _ _ s)
  | s == "new" = createIssue t
  | s == "updated" = updateIssue t

parseConfig :: Maybe Value -> Maybe Config
parseConfig cfg =
  Config <$> (cfg ^. key "issue_creator" . key "user" . asText)
         <*> (cfg ^. key "issue_creator" . key "password" . asText)
         <*> (cfg ^. key "repo" . key "repo" . asText)
         <*> (cfg ^. key "repo" . key "user" . asText)

push :: IO ()
push = do
  cfgTxt <- BS.readFile cfgPath
  case decode cfgTxt >>= parseConfig of
    Just cfg -> do
      conn <- open dbPath
      todos <- query conn "SELECT * FROM todos" ()
      flip runReaderT (conn, cfg) $ mapM_ createOrUpdate todos
    Nothing -> die ("Make sure " <> cfgPath
                    <> " is valid json and has the required fields")
