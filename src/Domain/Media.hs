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
           | Media { entityId :: EntityId
                   , mediaId :: MediaIdentifier
                   , mediaClass :: MediaClass
                   , isDeleted :: Bool
                   }

data MediaCommand = AddMedia MediaIdentifier
             | DeleteMedia MediaIdentifier

data MediaEvent = MediaWasAdded EntityId MediaIdentifier MediaClass
                | MediaWasDeleted EntityId
                deriving (Eq, Show)

mediaEntity :: Media -> Entity Media -- MediaEvent
mediaEntity media = Entity {
  _entityId = entityId
--  apply = applyMediaEvent
  }

-- applyMediaEvent :: MediaEvent -> Media -> Either String Media
-- applyMediaEvent event media = case event of
--   MediaWasAdded id' mediaId mediaClass -> case media of
--     EmptyMedia -> Right $ Media 

-- For now, just return Photo
getMediaClassForFile :: MediaIdentifier -> MediaClass
getMediaClassForFile _ = Photo

mediaHandler :: MediaCommand -> IO [MediaEvent]
mediaHandler (AddMedia mediaId) = pure [MediaWasAdded id mediaId mediaClass]
  where id = hashToHexString mediaId -- the aggregate ID will simply be the sha1 hash of the MediaIdentifier
        hashToHexString = Data.List.concatMap (printf "%02x") . BS.unpack . SHA1.hash . BS.pack
        mediaClass = getMediaClassForFile mediaId
mediaHandler (DeleteMedia mediaId) = pure [MediaWasDeleted mediaId]
