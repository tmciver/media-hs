module Main where
 
import Test.Hspec
import Media.Domain.Model.Media
import EventSourcing.Entity
import Media.Test.Todo
import Data.DateTime (fromGregorian', addMinutes, getCurrentTime)
import System.Random (randomIO)

randomIdentifier :: IO String
randomIdentifier = show <$> (randomIO :: IO Int)
 
main :: IO ()
main = hspec $ do
  describe "Tests for event application." $ do
    it "should apply the `MediaWasAdded` event to `EmptyMedia`" $ do
      eid <- randomIdentifier
      let ini = EmptyMedia
          mid = "/media/" ++ eid
          mClass = Photo
          event = MediaWasAdded eid mid mClass
          eitherMedia = apply ini event
      eitherMedia `shouldBe` Right (Media eid mid mClass False)

    it "should result in an error when applying the `MediaWasAdded` event to non-`EmptyMedia`" $ do
      eid <- randomIdentifier
      let ini = EmptyMedia
          mid = "/media/" ++ eid
          mClass = Photo
          event = MediaWasAdded eid mid mClass
          eitherMedia = apply ini event
          error = eitherMedia >>= (\media -> apply media event)
      error `shouldBe` Left "MediaWasAdded event can only be applied to EmptyMedia."

    it "should apply the `MediaWasDeleted` event to non-`EmptyMedia`" $ do
      eid <- randomIdentifier
      let mid = "/media/" ++ eid
          mClass = Photo
          nonDeletedMedia = Media eid mid mClass False
          event = MediaWasDeleted eid
          eitherMedia = apply nonDeletedMedia event
      eitherMedia `shouldBe` Right (Media eid mid mClass True)

    it "should result in an error when applying the `MediaWasDeleted` event to deleted Media" $ do
      eid <- randomIdentifier
      let mid = "/media/" ++ eid
          mClass = Photo
          deletedMedia = Media eid mid mClass True
          event = MediaWasDeleted eid
          eitherMedia = apply deletedMedia event
      eitherMedia `shouldBe` Left "Attempted to delete media that was already deleted."

    -- it "should return a list containing a single event: MediaWasAdded" $ do
    --   let command = AddMedia path
    --       path = "/home/user/media/photo1.jpg"
    --       id' = "d6432bddab5d2ef255a1922ee45f85ac89636ec8"
    --       mClass' = Photo
    --       f = undefined
    --   events <- handle f EmptyMedia command
    --   events `shouldBe` Right [MediaWasAdded id' path mClass']

    -- it "should return a list containing a single event: MediaWasDeleted" $ do
    --   let path = "/home/user/media/photo1.jpg"
    --       eid = "d6432bddab5d2ef255a1922ee45f85ac89636ec8"
    --       mClass' = Photo
    --       media = Media eid path mClass' False
    --   events <- handle media (DeleteMedia eid)
    --   events `shouldBe` Right [MediaWasDeleted eid]

  describe "Event Sourcing Tests" $ do
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
