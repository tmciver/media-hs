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
           | Media { mediaEntityId :: EntityId
                   , mediaId :: MediaIdentifier
                   , mediaClass :: MediaClass
                   , isDeleted :: Bool
                   }
           deriving (Eq, Show)

instance Entity Media where
  data Command Media = AddMedia EntityId MediaIdentifier
                     | DeleteMedia EntityId
                     deriving (Eq, Show)
  data Event Media = MediaWasAdded EntityId MediaIdentifier MediaClass
                   | MediaWasDeleted EntityId
                   deriving (Eq, Show)

  entityId e = case e of
    EmptyMedia -> Nothing
    m -> Just $ mediaId m

  init = EmptyMedia

  apply media event = case event of
    MediaWasAdded id' mediaId' mediaClass' -> case media of
      EmptyMedia -> Right $ Media { mediaEntityId = id'
                                  , mediaId = mediaId'
                                  , mediaClass = mediaClass'
                                  , isDeleted = False }
      Media _ _ _ _ -> Left "MediaWasAdded event can only be applied to EmptyMedia."
    MediaWasDeleted id' -> case media of
      EmptyMedia -> Left "MediaWasDeleted event cannot be applied to EmptyMedia."
      Media id' mediaId' mediaClass' _ -> Right $ Media { mediaEntityId = id'
                                                        , mediaId = mediaId'
                                                        , mediaClass = mediaClass'
                                                        , isDeleted = True
                                                        }

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
