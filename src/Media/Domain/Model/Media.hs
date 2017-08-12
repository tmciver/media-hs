{-# LANGUAGE TypeFamilies #-}

module Media.Domain.Model.Media
       ( Command(..)
       , Event(..)
       , MediaClass(..)
       , Media(..)
       ) where

import Data.List
import Text.Printf
import Data.ByteString.Char8 as BS
import Crypto.Hash.SHA1 as SHA1
import EventSourcing.Entity

-- For now, media will be identified by it's file system path.
type MediaIdentifier = FilePath

-- Media can be one of the following types
data MediaClass = Photo | Video | Audio
                deriving (Eq, Show)
data Media = EmptyMedia
           | Media { mediaEntityId :: String
                   , mediaId :: MediaIdentifier
                   , mediaClass :: MediaClass
                   , isDeleted :: Bool
                   }
           deriving (Eq, Show)

instance Entity Media where
  data EntityId Media = EntityIdMedia String
  data Command Media = AddMedia String MediaIdentifier
                     | DeleteMedia String
                     deriving (Eq, Show)
  data Event Media = MediaWasAdded String MediaIdentifier MediaClass
                   | MediaWasDeleted String
                   deriving (Eq, Show)

  entityId = EntityIdMedia . mediaEntityId

  init = EmptyMedia

  apply media event = case (media, event) of
    (EmptyMedia, MediaWasAdded id' mediaId' mediaClass') ->
      Right $ Media { mediaEntityId = id'
                    , mediaId = mediaId'
                    , mediaClass = mediaClass'
                    , isDeleted = False }
    (_, MediaWasAdded _ _ _) ->
      Left "MediaWasAdded event can only be applied to EmptyMedia."
    (Media entityId' id' class' False, MediaWasDeleted id'') | id' == id'' ->
      Right $ Media { mediaEntityId = entityId'
                    , mediaId = id'
                    , mediaClass = class'
                    , isDeleted = True
                    }
    (Media _ _ _ False, MediaWasDeleted _) ->
      Left "Attempted to delete media that was already deleted."
    (EmptyMedia, MediaWasDeleted _) ->
      Left "MediaWasDeleted event cannot be applied to EmptyMedia."
    (_, MediaWasDeleted _) ->
      Left "Identifiers did not match when attempting to delete media."

  handle EmptyMedia (AddMedia eid mid) = (pure . pure) [MediaWasAdded eid mid mediaClass']
    where eid = hashToHexString mid -- the aggregate ID will simply be the sha1 hash of the MediaIdentifier
          hashToHexString = Data.List.concatMap (printf "%02x") . BS.unpack . SHA1.hash . BS.pack
          mediaClass' = getMediaClassForFile mid
  handle _ (AddMedia _ _) = pure $ Left "Cannot apply the `AddMedia` command to non-empty media."
  handle (Media eid mid c deleted) (DeleteMedia eid') = if eid == eid' then
                                                          (pure . pure) [MediaWasDeleted eid]
                                                        else
                                                          pure $ Left "Entity ID mismatch found when attempting to handle the `DeleteMedia` command."
  handle _ (DeleteMedia _) = pure $ Left "Attempted to delete empty media."

-- For now, just return Photo
getMediaClassForFile :: MediaIdentifier -> MediaClass
getMediaClassForFile _ = Photo
