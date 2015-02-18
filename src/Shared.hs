{-# LANGUAGE OverloadedStrings #-}
module Shared where

import Prelude hiding (FilePath)
import Data.Monoid ((<>))
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import qualified Data.Text as T
import Filesystem.Path.CurrentOS hiding (empty, null)

dbPath :: String
dbPath = ".git/hooks/todos.db"

fromRight :: Either a b -> b
fromRight (Right x) = x

data Todo = Todo { _file :: FilePath
                 , _line :: Int
                 , _todo :: T.Text
                 , _status :: T.Text
                 , _number :: Maybe Int
                 } deriving (Show, Eq)

instance ToField FilePath where
  toField = toField . fromRight . toText

instance FromRow Todo where
  fromRow = Todo <$> (fromText <$> field)
                 <*> field
                 <*> field
                 <*> field
                 <*> field

todoMsg :: T.Text -> Todo -> T.Text
todoMsg msg (Todo fp ln td _ _) = "[" <> msg <> "] " <>
                                   fromRight (toText fp) <>
                                   ":" <> T.pack (show ln) <>
                                   " " <> td
