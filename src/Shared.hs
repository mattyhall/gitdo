{-# LANGUAGE OverloadedStrings #-}
module Shared where

import Prelude hiding (FilePath)
import Data.Monoid ((<>))
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import qualified Data.Text as T
import Filesystem.Path.CurrentOS hiding (empty, null)

dbPath :: String
dbPath = ".git/hooks/todos.db"

fromRight :: Either a b -> b
fromRight (Right x) = x

data Status = New
            | Updated
            | Synced
            | Deleted
            | None
            deriving (Show, Eq)

instance ToField Status where
  toField New = toField ("new" :: String)
  toField Updated = toField ("updated" :: String)
  toField Synced = toField ("synced" :: String)
  toField Deleted = toField ("deleted" :: String)

fromString :: String -> Status
fromString s =
  case s of
    "new" -> New
    "updated" -> Updated
    "synced" -> Synced
    "deleted" -> Deleted

instance FromField Status where
  fromField f = fromString <$> fromField f

data Todo = Todo { _file :: FilePath
                 , _line :: Int
                 , _todo :: T.Text
                 , _status :: Status
                 , _number :: Maybe Int
                 } deriving (Show)

instance Eq Todo where
  (Todo fp ln td _ _) == (Todo fp' ln' td' _ _) = fp == fp' && ln == ln'
                                                            && td == td

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
