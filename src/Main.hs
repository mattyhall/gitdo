{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid ((<>))
import Turtle
import qualified Data.Text as T
import Database.SQLite.Simple
import Options.Applicative
import Shared
import Commit
import Push

createTable :: IO ()
createTable = do
  conn <- liftIO $ open dbPath
  execute conn ("CREATE TABLE todos " <>
    "(file TEXT, line INT, todo TEXT, status TEXT, number INT);") ()

data Command = AddHooks
             | Commit
             | Reset
             | Push
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
  <>
  command "reset"
          (info (helper <*> pure Reset)
            (progDesc "Delete new todos"))
  <>
  command "push"
          (info (helper <*> pure Push)
            (progDesc "Create/update issues on github"))

run :: Command -> IO ()
run (AddHooks) = do
  output ".git/hooks/post-commit" "#!/bin/bash\ngitdo commit"
  shell "chmod +x .git/hooks/post-commit" empty
  putStrLn "Created post-commit hook"
  createTable
  putStrLn "Created database"

run (Commit) = commit
run (Push) = push
run (Reset) = do
  conn <- open dbPath
  execute conn "DELETE FROM todos WHERE status=?" (Only "new" :: Only String)
  putStrLn "Deleted new todos"

main  :: IO ()
main = execParser (info (helper <*> opts) $ progDesc "Todo comments to issues")
       >>= run
