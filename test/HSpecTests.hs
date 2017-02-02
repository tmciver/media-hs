module Main where
 
import Test.Hspec
import Media.Domain.Model.Media
import EventSourcing.Entity
 
main :: IO ()
main = hspec $ do
  describe "Test for `mediaHandler`." $ do
    it "should return a list containing a single event: MediaWasAdded" $ do
      let command = AddMedia path
          path = "/home/user/media/photo1.jpg"
          id' = "d6432bddab5d2ef255a1922ee45f85ac89636ec8"
          mediaClass = Photo
      events <- handle command
      events `shouldBe` Right [MediaWasAdded id' path mediaClass]

    it "should return a list containing a single event: MediaWasDeleted" $ do
      let id' = "d6432bddab5d2ef255a1922ee45f85ac89636ec8"
      events <- handle $ DeleteMedia id'
      events `shouldBe` Right [MediaWasDeleted id']
