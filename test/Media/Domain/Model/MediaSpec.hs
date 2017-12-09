module Media.Domain.Model.MediaSpec where

import Test.Hspec
import Media.Domain.Model.Media
import EventSourcing.Entity
import System.Random (randomIO)

randomIdentifier :: IO String
randomIdentifier = show <$> (randomIO :: IO Int)

spec :: Spec
spec = describe "Tests for event application." $ do
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
