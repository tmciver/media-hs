module Main where
 
import Test.Hspec
import Media.Domain.Model.Media
import EventSourcing.Entity
import Media.Test.Todo
import Data.DateTime (fromGregorian', addMinutes)
 
main :: IO ()
main = hspec $ do
  describe "Test for `mediaHandler`." $ do
    it "should return a list containing a single event: MediaWasAdded" $ do
      let command = AddMedia path
          path = "/home/user/media/photo1.jpg"
          id' = "d6432bddab5d2ef255a1922ee45f85ac89636ec8"
          mediaClass' = Photo
      events <- handle EmptyMedia command
      events `shouldBe` Right [MediaWasAdded id' path mediaClass']

    it "should return a list containing a single event: MediaWasDeleted" $ do
      let path = "/home/user/media/photo1.jpg"
          eid = "d6432bddab5d2ef255a1922ee45f85ac89636ec8"
          mediaClass' = Photo
          media = Media eid path mediaClass' False
      events <- handle media (DeleteMedia eid)
      events `shouldBe` Right [MediaWasDeleted eid]

  describe "Todo Tests" $ do
    describe "Todo EventStore Tests" $ do
      it "should 'get' the same events that it 'save's for a given entity ID" $ do
        let juneSixth = fromGregorian' 2017 6 6
            oneDayInMinutes = 60 * 24
            juneSeventh = addMinutes oneDayInMinutes juneSixth
            savedEvents = [ TodoWasCreated "123" "Buy milk" juneSixth
                          , DueDateWasChanged "123" juneSeventh]
        todoEventStore <- newTodoEventStore
        _ <- save todoEventStore "123" savedEvents
        retrievedEvents <- get todoEventStore "123"
        retrievedEvents `shouldBe` savedEvents

    describe "Tests for Event Sourcing repository function `getEntityById`" $ do
      it "should return the correct hydrated entity" $ do
        let juneSixth = fromGregorian' 2017 6 6
            oneDayInMinutes = 60 * 24
            juneSeventh = addMinutes oneDayInMinutes juneSixth
            savedEvents = [ TodoWasCreated "123" "Buy milk" juneSixth
                          , DueDateWasChanged "123" juneSeventh]
        todoEventStore <- newTodoEventStore
        _ <- save todoEventStore "123" savedEvents
        let expectedTodo = Todo "123" "Buy milk" juneSeventh False
        Right(todo) <- getEntityById todoEventStore "123"
        -- _ <- print x
        expectedTodo `shouldBe` todo
