{-# LANGUAGE OverloadedStrings #-}
module Commit where

import Prelude hiding (FilePath)
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Database.SQLite.Simple hiding (fold)
import Turtle
import qualified Control.Foldl as F
import Shared

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
      return (Todo file n (T.strip v) None Nothing)
    Nothing  -> liftIO (die $ unknownProgrammingLanguage file)

insertTodo :: Connection -> Todo -> Shell ()
insertTodo conn t@(Todo fp ln td _ _) = do
  let q = "INSERT INTO todos (file, line, todo, status) VALUES (?, ?, ?, ?)"
  liftIO (TIO.putStrLn $ todoMsg "NEW" t)
  liftIO $ execute conn q (fp, ln, td, New)

updateTodo :: Connection -> Todo -> Todo -> Shell ()
updateTodo conn t@(Todo fp ln td _ _) (Todo _ _ _ status _) = do
  let s = if status == New
            then New
            else Updated
  let q = "UPDATE todos SET file=?, line=?, status=? WHERE todo=?"
  liftIO (TIO.putStrLn $ todoMsg "UPDATE" t)
  liftIO $ execute conn q (fp, ln, s, td)

updateDatabase :: Connection -> Shell Todo -> Shell ()
updateDatabase conn todos = do
  todo@(Todo fp ln td _ _) <- todos
  qs <- liftIO $ query conn "SELECT * FROM todos WHERE todo=?"
          (Only td) :: Shell [Todo]
  if null qs
    then insertTodo conn todo
    else updateTodo conn todo (head qs)

updateMissingTodos :: Connection -> Shell Todo -> T.Text -> Shell ()
updateMissingTodos conn todos fp = do
  records <- liftIO $ query conn "SELECT * FROM todos WHERE file=?" (Only fp)
  todos' <- liftIO $ fold todos F.list
  flip mapM_ (filter (`notElem` todos') records) $ \t@(Todo fp ln td _ _) -> do
    liftIO (TIO.putStrLn $ todoMsg "DELETE" t)
    liftIO $ execute conn
                     "UPDATE todos SET status=? WHERE file=? AND todo=?"
                     (Deleted, fp, td)

commit :: IO ()
commit = output "/dev/null" $ do
  let files = inshell "git diff --name-only HEAD^ HEAD" empty
  conn <- liftIO $ open dbPath
  file <- grep filenamePattern files
  let todos = handleFile (fromText file)
  updateDatabase conn todos
  updateMissingTodos conn todos file 
  return ""
