module Domain.Media
       ( --AggregateId
         --, Aggregate
         MediaCommand(..)
       , MediaClass(..)
       , Media
       , MediaEvent(..)
       , mediaHandler
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

data MediaCommand = AddMedia MediaIdentifier
                  | DeleteMedia MediaIdentifier

data MediaEvent = MediaWasAdded EntityId MediaIdentifier MediaClass
                | MediaWasDeleted EntityId
                deriving (Eq, Show)

mediaEntity :: Media -> Entity Media MediaCommand MediaEvent
mediaEntity media = Entity {
  _entityId = mediaEntityId,
  _init = EmptyMedia,
  _apply = applyMediaEvent,
  _handle = mediaHandler
  }

applyMediaEvent :: Media -> MediaEvent -> Either String Media
applyMediaEvent media event = case event of
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
                                                    , isDeleted = True }

-- For now, just return Photo
getMediaClassForFile :: MediaIdentifier -> MediaClass
getMediaClassForFile _ = Photo

mediaHandler :: MediaCommand -> IO (Either String [MediaEvent])
mediaHandler (AddMedia mediaId') = (pure . pure) [MediaWasAdded id mediaId' mediaClass']
  where id = hashToHexString mediaId' -- the aggregate ID will simply be the sha1 hash of the MediaIdentifier
        hashToHexString = Data.List.concatMap (printf "%02x") . BS.unpack . SHA1.hash . BS.pack
        mediaClass' = getMediaClassForFile mediaId'
mediaHandler (DeleteMedia mid) = (pure . pure) [MediaWasDeleted mid]
