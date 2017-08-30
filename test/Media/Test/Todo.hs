{-# LANGUAGE TypeFamilies #-}

module Media.Test.Todo ( Todo(..)
                       , Command(..)
                       , Event(..)
                       , newTodoEventStore) where

import Data.DateTime (DateTime, startOfTime)
import EventSourcing.Entity
import Data.Map as Map
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)

-- Fields are:
--   ID
--   Text of todo
--   due date
--   bool indicating if it has been completed
data Todo = Todo String String DateTime Bool
          deriving (Eq, Show)

instance Entity Todo where
  type EntityId Todo = String

  data Command Todo = CreateTodo String DateTime
                    | CompleteTodo String -- ID
                    | ChangeDueDate String DateTime -- ID, due date
                    deriving (Eq, Show)

  data Event Todo = TodoWasCreated String String DateTime
                  | TodoWasCompleted String
                  | DueDateWasChanged String DateTime
                  deriving (Eq, Show)

  entityId (Todo id' _ _ _) = id'
  init = Todo "" "Nothing to do." startOfTime False
  apply = applyTodoEvent
  handle _ _ = pure (pure [])


applyTodoEvent :: Todo -> Event Todo -> Either String Todo
applyTodoEvent _ (TodoWasCreated id' text dueDate) = Right $ Todo id' text dueDate False
applyTodoEvent (Todo id' text dueDate _) (TodoWasCompleted _) = Right $ Todo id' text dueDate True
applyTodoEvent (Todo id' text _ False) (DueDateWasChanged _ newDueDate) = Right $ Todo id' text newDueDate False
applyTodoEvent todo event = Left $ "Could not apply event " ++ (show event) ++ " to Todo " ++ (show todo)

type TodoMap = Map (EntityId Todo) [Event Todo]

newTodoEventStore :: IO (EventStore Todo)
newTodoEventStore = do
  ref <- newIORef (Map.empty :: TodoMap)
  return $ EventStore (getTodoEvents ref) (saveTodoEvents ref)

getTodoEvents :: IORef TodoMap -> EntityId Todo -> IO [Event Todo]
getTodoEvents ref id' = do
  m <- readIORef ref
  let events = fromMaybe [] (Map.lookup id' m)
  return events

saveTodoEvents :: IORef TodoMap -> EntityId Todo -> [Event Todo] -> IO (Either String ())
saveTodoEvents ref id' events = do
  m <- readIORef ref
  let m' = Map.insertWith (++) id' events m
  _ <- writeIORef ref m'
  return $ Right ()
