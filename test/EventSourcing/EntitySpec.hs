module EventSourcing.EntitySpec where

import Test.Hspec
import EventSourcing.Entity
import Media.Test.Todo
import Data.DateTime (fromGregorian', addMinutes, getCurrentTime)

spec :: Spec
spec = describe "Event Sourcing Tests" $ do
    describe "EventStore Tests" $ do
      it "should 'get' the same events that it 'save's for a given entity ID" $ do
        let juneSixth = fromGregorian' 2017 6 6
            oneDayInMinutes = 60 * 24
            juneSeventh = addMinutes oneDayInMinutes juneSixth
            savedEvents = [ TodoWasCreated "123" "Buy milk" juneSixth
                          , DueDateWasChanged "123" juneSeventh]
            savedEventList = EventList "123" savedEvents
        todoEventStore <- newTodoEventStore
        _ <- save todoEventStore savedEventList
        (EventList _ retrievedEvents) <- get todoEventStore "123"
        retrievedEvents `shouldBe` savedEvents

      it "should return the empty list for and entity that has no events" $ do
        todoEventStore <- newTodoEventStore
        (EventList _ retrievedEvents) <- get todoEventStore "123"
        retrievedEvents `shouldBe` []

    describe "Repository Tests" $ do
      it "should return the correct hydrated entity" $ do
        let juneSixth = fromGregorian' 2017 6 6
            oneDayInMinutes = 60 * 24
            juneSeventh = addMinutes oneDayInMinutes juneSixth
            savedEvents = (EventList "123" [ TodoWasCreated "123" "Buy milk" juneSixth
                                           , DueDateWasChanged "123" juneSeventh])
        todoEventStore <- newTodoEventStore
        _ <- save todoEventStore savedEvents
        let expectedTodo = Todo "123" "Buy milk" juneSeventh False
        Right(todo) <- getEntityById (get todoEventStore) "123"
        expectedTodo `shouldBe` todo

      it "should return an error for an entity that has no events" $ do
        todoEventStore <- newTodoEventStore
        getEntityById (get todoEventStore) "123" `shouldReturn` Left "No events for entity ID"

    describe "Command-handling Tests" $ do
      it "should load the event store with the appropriate events" $ do
        currentTime <- getCurrentTime
        todoEventStore <- newTodoEventStore
        let command = CreateTodo "Get rash checked out" currentTime
            commandHandler = handleCommand todoEventStore todoCommandHandler todoEventListener
        Right id' <- commandHandler command
        (EventList _ events) <- get todoEventStore id'
        events `shouldBe` [TodoWasCreated id' "Get rash checked out" currentTime]
