module Main where
 
import Test.Hspec
import Media.Domain.Model.Media
import EventSourcing.Entity
import Media.Test.Todo
import Data.DateTime (fromGregorian')
 
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

  describe "Tests for Event Sourcing repository function `getEntityById`" $ do
    it "should return the correct hydrated entity" $ do
      let juneSeventh = fromGregorian' 2017 6 7
          expectedTodo = Todo "123" "Buy milk" juneSeventh False
      Right(todo) <- getEntityById todoEventStore "123"
      expectedTodo `shouldBe` todo
