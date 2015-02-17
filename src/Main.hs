{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (FilePath)
import Data.List (lookup)
import Data.Monoid ((<>))
import qualified Data.ByteString as B
import Turtle
import qualified Data.Text as T
import Filesystem.Path.CurrentOS hiding (empty, null)
import Database.SQLite.Simple
import Options.Applicative
import Shared
import Commit

createTable :: IO ()
createTable = do
  conn <- liftIO $ open dbPath
  execute conn "CREATE TABLE todos (file TEXT, line INT, todo TEXT, status TEXT);" ()

data Command = AddHooks
             | Commit
             deriving (Show, Eq)

opts :: Parser Command
opts = subparser $
  command "add-hooks"
          (info (helper <*> pure AddHooks)
            (progDesc "Add scripts to .git/hooks"))
  <>
  command "commit"
          (info (helper <*> pure Commit)
            (progDesc "Commit hook. Adds todos to database"))

run :: Command -> IO ()
run (AddHooks) = do
  output ".git/hooks/post-commit" "#!/bin/bash\ngitdo commit"
  shell "chmod +x .git/hooks/post-commit" empty
  putStrLn "Created post-commit hook"
  createTable
  putStrLn "Created database"

run (Commit) = commit

main  :: IO ()
main = execParser (info (helper <*> opts) $ progDesc "Todo comments to issues") >>= run
