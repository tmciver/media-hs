{-# LANGUAGE TypeFamilies #-}

module Media.Test.Todo ( Todo(..)
                       , Command(..)
                       , Event(..)
                       , EntityId(..)
                       , todoEventStore) where

import Data.DateTime (DateTime, fromGregorian', addMinutes, startOfTime)
import EventSourcing.Entity

-- Fields are:
--   ID
--   Text of todo
--   due date
--   bool indicating if it has been completed
data Todo = Todo String String DateTime Bool
          deriving (Eq, Show)

instance Entity Todo where
  data EntityId Todo = TodoId String

  data Command Todo = CreateTodo String DateTime
                    | CompleteTodo String -- ID
                    | ChangeDueDate String DateTime -- ID, due date
                    deriving (Eq, Show)

  data Event Todo = TodoWasCreated String String DateTime
                  | TodoWasCompleted String
                  | DueDateWasChanged String DateTime
                  deriving (Eq, Show)

  entityId (Todo id' _ _ _) = TodoId id'
  init = Todo "" "Nothing to do." startOfTime False
  apply = applyTodoEvent
  handle _ _ = pure (pure [])


applyTodoEvent :: Todo -> Event Todo -> Either String Todo
applyTodoEvent _ (TodoWasCreated id' text dueDate) = Right $ Todo id' text dueDate False
applyTodoEvent (Todo id' text dueDate _) (TodoWasCompleted _) = Right $ Todo id' text dueDate True
applyTodoEvent (Todo id' text _ False) (DueDateWasChanged _ newDueDate) = Right $ Todo id' text newDueDate False
applyTodoEvent todo event = Left $ "Could not apply event " ++ (show event) ++ " to Todo " ++ (show todo)

addEvents :: EntityId Todo -> [Event Todo] -> IO (Either String ())
addEvents _ _ = pure $ pure ()

getEvents :: EntityId Todo -> IO [Event Todo]
getEvents _ = let juneSixth = fromGregorian' 2017 6 6
                  oneDayInMinutes = 60 * 24
                  juneSeventh = addMinutes oneDayInMinutes juneSixth
              in
               pure [ TodoWasCreated "123" "Buy milk" juneSixth
                    , DueDateWasChanged "123" juneSeventh]

todoEventStore :: EventStore Todo
todoEventStore = EventStore getEvents addEvents

