module Main (main) where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time

main :: IO ()
main = do
  print "Enter a command"
  command <- getLine
  performCommand command

dbName :: String
dbName = "tools.db"

--L41-4
data Tool = Tool {
    toolId :: Int
  , name :: String
  , description :: String
  , lastReturned :: Day
  , timesBorrowed :: Int
}

--L41-5
data User = User {
    userId :: Int
  , userName :: String
}

--L41-6
instance Show User where
  show user = mconcat
    [
        show $ userId user
      , ".) "
      , userName user
    ]

instance Show Tool where
  show tool = mconcat
    [
        show $ toolId tool
      , ".) "
      , name tool
      , "\n description "
      , description tool
      , "\n last returned "
      , show $ lastReturned tool
      , "\n times borrowed: "
      , show $ timesBorrowed tool
      , "\n"
    ]

--L41-7
-- addUser :: String -> IO ()
-- addUser userName = do
--   conn <- open "tools.db"
--   execute conn "INSERT INTO users (username) VALUES (?)" (Only userName)
--   print "user added"
--   close conn

--L41-8
withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
  conn <- open dbName
  action conn
  close conn

--L41-9
checkout :: Int -> Int -> IO ()
checkout userId toolId = withConn dbName $ \ conn -> execute
  conn
  "INSERT INTO checkedout (user_id, tool_id) VALUES (?, ?);"
  (userId, toolId)

--L41-10
-- class FromRow a where
--   fromRow :: RowParser a

--L41-11
instance FromRow User where
  fromRow = User <$> field <*> field

instance FromRow Tool where
  fromRow = Tool <$> field <*> field <*> field <*> field <*> field

--L41-12
printUsers :: IO ()
printUsers = withConn dbName $ \ conn -> do
  res <- query_ conn "SELECT * FROM users;" :: IO [User]
  mapM_ print res

--L41-13
printToolQuery :: Query -> IO ()
printToolQuery q = withConn dbName $ \ conn -> do
  res <- query_ conn q :: IO [Tool]
  mapM_ print res

printTools :: IO ()
printTools = printToolQuery "SELECT * FROM tools;"

printAvailable :: IO ()
printAvailable = printToolQuery $
  mconcat [
      "SELECT * FROM tools "
    , "WHERE id NOT IN ("
    , " SELECT tool_id "
    , " FROM checkedout"
    , ");"
  ]

printCheckedout :: IO ()
printCheckedout = printToolQuery $
  mconcat [
      "SELECT * FROM tools "
    , "WHERE id IN ("
    , " SELECT tool_id "
    , " FROM checkedout"
    , ");"
  ]

--L41-14
selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId = do
  res <- query conn
    "SELECT * FROM tools WHERE id = (?);"
    (Only toolId) :: IO [Tool]
  return $ firstOrNothing  res

firstOrNothing :: [a] -> Maybe a
firstOrNothing [] = Nothing
firstOrNothing (x : _) = Just x

--L41-15
updateTool :: Tool -> Day -> Tool
updateTool tool date = tool {
      lastReturned = date
    , timesBorrowed = 1 + timesBorrowed tool
  }

--L41-16
updateOrWarn :: Maybe Tool -> IO ()
updateOrWarn Nothing = print "id not found"
updateOrWarn (Just tool) = withConn dbName $ \ conn -> do
  let q =   mconcat [
            "UPDATE tools SET "
          , "lastReturned = ? "
          , ", timesBorrowed = ? "
          , "WHERE ID = ?;"
        ]
  execute conn q (lastReturned tool, timesBorrowed tool, toolId tool)
  print "tool updated"

--L41-17
updateToolTable :: Int -> IO ()
updateToolTable toolId = withConn dbName$ \ conn -> do
  tool <- selectTool conn toolId
  currentDay <- utctDay <$> getCurrentTime
  let updatedTool = updateTool <$> tool <*> pure currentDay
  updateOrWarn updatedTool

--L41-18
checkin :: Int -> IO ()
checkin toolId = withConn dbName $ \ conn -> do
  execute conn "DELETE FROM checkedout WHERE tool_id = (?);" (Only toolId)

--L41-19
checkinAndUpdate :: Int -> IO ()
checkinAndUpdate toolId = do
  checkin toolId
  updateToolTable toolId

--L41-20
promptAndAddUser :: IO ()
promptAndAddUser = do
  print "Enter new user name"
  userName <- getLine
  addUser userName

promptAndCheckout :: IO ()
promptAndCheckout = do
  print "Enter the id of the user"
  userId <- read <$> getLine
  print "Enter the id of the tool"
  toolId <- read <$> getLine
  checkout userId toolId

promptAndCheckin :: IO ()
promptAndCheckin = do
  print "Enter the id of the tool"
  toolId <- read <$> getLine
  checkinAndUpdate toolId

--L41-21
performCommand :: String -> IO ()
performCommand command
  | command == "users" = printUsers >> main
  | command == "tools" = printTools >> main
  | command == "adduser" = promptAndAddUser >> main
  | command == "addtool" = promptAndAddTool >> main --Q41-2
  | command == "checkout" = promptAndCheckout >> main
  | command == "checkin" = promptAndCheckin >> main
  | command == "in" = printAvailable >> main
  | command == "out" = printCheckedout >> main
  | command == "quit" = print "bye!"
  | otherwise = print "Sorry command not found" >> main



--QC41-1
-- 文字列を結合するにあたって++よりもmconcatが優先されるのはなぜか。
-- 3 mconcatを使用すると、Text型を使ったリファクタリングが容易になるから

--QC41-2
addUser :: String -> IO ()
addUser userName = withConn dbName $ \ conn -> do
  execute conn "INSERT INTO users (username) VALUES (?);" (Only userName)
  print "user added"

--QC41-3
-- queryとquery_の2つが必要な理由は、なぜか。

--QC41-4
-- >>の代わりに>>=を使用できないのはなぜか。
-- 2 >>はmainが引数を受け取ることを示唆するが、mainは引数を受け取らないから


--Q41-1
addTool :: String -> String -> IO ()
addTool toolName description = withConn dbName $ \ conn -> do
  let q =   mconcat [
            "INSERT INTO tools "
          , "(name, description, lastReturned, timesBorrowed) "
          , "VALUES (?, ?, '2023-01-01', 0);"
        ]
  execute conn q (toolName, description)
  print "tool added"

--L41-2
promptAndAddTool :: IO ()
promptAndAddTool = do
  print "Enter new tool name"
  toolName <- getLine
  print "Enter tool description"
  description <- getLine
  addTool toolName description
