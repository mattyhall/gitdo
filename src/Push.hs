{-# LANGUAGE OverloadedStrings #-}
module Push where

import Control.Applicative
import Network.Wreq
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
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

cfgPath :: IsString a => a
cfgPath = ".git/hooks/gitdo.json"

createIssue :: Config -> Todo -> IO ()
createIssue cfg (Todo fp ln td _) = undefined

updateIssue :: Config -> Todo -> IO ()
updateIssue = undefined

createOrUpdate :: Config -> Todo -> IO ()
createOrUpdate cfg t@(Todo _ _ _ s)
  | s == "new" = createIssue cfg t
  | s == "updated" = updateIssue cfg t

parseConfig :: Maybe Value -> Maybe Config
parseConfig cfg =
  Config <$> (cfg ^. key "issue_creator" . key "user" . asText)
         <*> (cfg ^. key "issue_creator" . key "password" . asText)
         <*> (cfg ^. key "repo" . key "repo" . asText)
         <*> (cfg ^. key "repo" . key "user" . asText)

push :: IO ()
push = do
  cfgTxt <- BS.readFile cfgPath
  BS.putStrLn cfgTxt
  case decode cfgTxt >>= parseConfig of
    Just cfg -> do
      conn <- open dbPath
      todos <- query conn "SELECT * FROM todos" ()
      mapM_ (createOrUpdate cfg) todos
    Nothing -> die ("Make sure " <> cfgPath
                    <> " is valid json and has the required fields")
