{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (FilePath)
import Data.IORef
import Data.List (lookup)
import Data.Monoid ((<>))
import qualified Data.ByteString as B
import Turtle
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Filesystem.Path.CurrentOS hiding (empty, null)
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Options.Applicative

dbPath :: String
dbPath = ".git/hooks/todos.db"

data Todo = Todo { _file :: FilePath
                 , _line :: Int
                 , _todo :: T.Text
                 , _status :: T.Text
                 } deriving (Show, Eq)

instance ToField FilePath where
  toField = toField . fromRight . toText

instance FromRow Todo where
  fromRow = Todo <$> (fromText <$> field)
                 <*> field
                 <*> field
                 <*> field

fromRight :: Either a b -> b
fromRight (Right x) = x

filenamePattern :: Pattern T.Text
filenamePattern = selfless (plus dot) <> "." <> ("rs" <|> "hs" <|> "py")

commentPattern :: T.Text -> Pattern T.Text
commentPattern tok = do
  spaces
  text tok
  spaces
  text "TODO"
  optional ":"
  spaces
  plus anyChar

commentsLookupTable :: [(T.Text, Pattern T.Text)]
commentsLookupTable = [ ("rs", commentPattern "//")
                      , ("hs", commentPattern "--")
                      , ("py", commentPattern "#")]

matchWithLineNumber :: Pattern T.Text -> Shell T.Text -> Shell (Int, T.Text)
matchWithLineNumber pat sh = do
  ref <- liftIO (newIORef 1)
  line <- sh
  n <- liftIO (readIORef ref)
  liftIO $ writeIORef ref (n + 1)
  v:_ <- return (match pat line)
  return (n, v)

unknownProgrammingLanguage :: FilePath -> T.Text
unknownProgrammingLanguage fp = "Could not deduce programming language of "
                               <> fromRight (toText fp)

handleFile :: FilePath -> Shell Todo
handleFile file = do
  let pat = extension file >>= flip lookup commentsLookupTable
  case pat of
    Just pat -> do
      (n, v) <- matchWithLineNumber pat (input file)
      return (Todo file n v "")
    Nothing  -> liftIO (die $ unknownProgrammingLanguage file)

todoMsg :: T.Text -> Todo -> T.Text
todoMsg msg (Todo fp ln td _) = "[" <> msg <> "] " <>
                                fromRight (toText fp) <>
                                ":" <> T.pack (show ln) <>
                                " " <> td

insertTodo :: Connection -> Todo -> Shell ()
insertTodo conn t@(Todo fp ln td _) = do
  let q = "INSERT INTO todos (file, line, todo, status) VALUES (?, ?, ?, ?)"
  liftIO (TIO.putStrLn $ todoMsg "NEW" t)
  liftIO $ execute conn q (fp, ln, td, "new" :: T.Text)

updateTodo :: Connection -> Todo -> Todo -> Shell ()
updateTodo conn t@(Todo fp ln td _) (Todo _ _ _ status )= do
  let s = if status == "new"
            then "new"
            else "updated"
  let q = "UPDATE todos SET file=?, line=?, status=? WHERE todo=?"
  liftIO (TIO.putStrLn $ todoMsg "UPDATE" t)
  liftIO $ execute conn q (fp, ln, status, td)

updateDatabase :: Shell Todo -> Shell ()
updateDatabase todos = do
  todo@(Todo fp ln td _) <- todos
  conn <- liftIO $ open dbPath
  qs <- liftIO $ query conn "SELECT * FROM todos WHERE todo=?" (Only td) :: Shell [Todo]
  if null qs
    then insertTodo conn todo
    else updateTodo conn todo (head qs)

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

run (Commit) = view $ do
  let files = inshell "git diff --name-only HEAD^ HEAD" empty
  file <- grep filenamePattern files
  let todos = handleFile (fromText file)
  updateDatabase todos

main  :: IO ()
main = execParser (info (helper <*> opts) $ progDesc "Todo comments to issues") >>= run
