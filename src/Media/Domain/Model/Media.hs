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
  type EntityId Media = String
  data Command Media = AddMedia MediaIdentifier
                     | DeleteMedia String
                     deriving (Eq, Show)
  data Event Media = MediaWasAdded String MediaIdentifier MediaClass
                   | MediaWasDeleted String
                   deriving (Eq, Show)

  entityId = mediaEntityId
  init = EmptyMedia
  apply = applyMediaEvent

applyMediaEvent :: Media -> Event Media -> Either String Media
applyMediaEvent EmptyMedia (MediaWasAdded id' mediaId' mediaClass') =
  Right $ Media { mediaEntityId = id'
                , mediaId = mediaId'
                , mediaClass = mediaClass'
                , isDeleted = False }
applyMediaEvent _ (MediaWasAdded _ _ _) =
  Left "MediaWasAdded event can only be applied to EmptyMedia."
applyMediaEvent m@(Media eid _ _ False) (MediaWasDeleted eid') | eid == eid' =
  Right $ m { isDeleted = True }
applyMediaEvent (Media _ _ _ True) (MediaWasDeleted _) =
  Left "Attempted to delete media that was already deleted."
applyMediaEvent EmptyMedia (MediaWasDeleted _) =
  Left "MediaWasDeleted event cannot be applied to EmptyMedia."
applyMediaEvent _ (MediaWasDeleted _) =
  Left "Identifiers did not match when attempting to delete media."

-- type CommandHandler e = Command e -> IO (Either String (EventList e))

mediaCommandHandler :: CommandHandler Media
-- Eventually, this handler will have to accept the media data and put it
-- into a document DB. The ID from that DB will then be used as the mediaId.
mediaCommandHandler _ (AddMedia mid) = (pure . pure) (EventList eid [MediaWasAdded eid mid mediaClass'])
    where eid = hashToHexString mid -- the aggregate ID will simply be the sha1 hash of the MediaIdentifier
          hashToHexString = Data.List.concatMap (printf "%02x") . BS.unpack . SHA1.hash . BS.pack
          mediaClass' = getMediaClassForFile mid
mediaCommandHandler getEntity (DeleteMedia eid) = do
  eitherEntity <- getEntity eid
  pure $ eitherEntity >>= produceEvents
    where produceEvents media = if (isDeleted media)
                                then Left "Attempted to delete empty media."
                                else Right (EventList eid [MediaWasDeleted eid])

-- For now, just return Photo
getMediaClassForFile :: MediaIdentifier -> MediaClass
getMediaClassForFile _ = Photo
