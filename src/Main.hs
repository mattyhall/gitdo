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
             | Push PushOpts
             deriving (Show, Eq)

txtOption :: Mod OptionFields String -> Parser T.Text
txtOption opts = T.pack <$> strOption opts

pushOpts :: Parser Command
pushOpts = Push <$>
  (PushOpts <$> txtOption
                 (  long "iuser"
                 <> short 'i'
                 <> metavar "ISSUE_USER"
                 <> help "User that will create the issue")
           <*> txtOption
                 (  long "password"
                 <> short 'p'
                 <> metavar "ISSUE_PSWD"
                 <> help "Password of ISSUE_USER")
           <*> txtOption
                 (  long "user"
                 <> short 'u'
                 <> metavar "USER"
                 <> help "The user who owns the repo")
           <*> txtOption
                 (  long "repo"
                 <> short 'r'
                 <> metavar "REPO"
                 <> help "The repo to create the issues in"))

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
          (info (helper <*> pushOpts)
            (progDesc "Create/update issues on github"))

run :: Command -> IO ()
run (AddHooks) = do
  output ".git/hooks/post-commit" "#!/bin/bash\ngitdo commit"
  shell "chmod +x .git/hooks/post-commit" empty
  putStrLn "Created post-commit hook"
  output ".git/hooks/pre-push"
         "#!/bin/bash\ngitdo push -i IUSER -p PSWD -u USER -r REPO"
  shell "chmod +x .git/hooks/pre-push" empty
  putStrLn "Created pre-push hook"
  createTable
  putStrLn "Created database"

run (Commit) = commit
run (Push opts) = push opts
run (Reset) = do
  conn <- open dbPath
  execute conn "DELETE FROM todos WHERE status=?" (Only "new" :: Only String)
  putStrLn "Deleted new todos"

main  :: IO ()
main = execParser (info (helper <*> opts) $ progDesc "Todo comments to issues")
       >>= run
