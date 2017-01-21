module Domain.Media
       ( AggregateId
       , Aggregate
       , Command(..)
       , MediaClass(..)
       , Media
       , Event(..)
       , mediaHandler
       ) where

import Data.List
import Text.Printf
import Data.ByteString.Char8 as BS
import Crypto.Hash.SHA1 as SHA1

type AggregateId = String
data Aggregate a = Aggregate { aggregateId :: AggregateId
                             , aggregateData :: a
                             }

-- Question: how do we identify a media file? I heard somewhere that large files
-- like photos and videos should not be stored in the events themselves but
-- should be put into a document repository. In that case I suppose we'd have
-- some kind of ID or URL to the file that we could store in the events.
-- For now, we'll just use a file system path.
type MediaIdentifier = FilePath

-- Media can be one of the following types
data MediaClass = Photo | Video | Audio
                deriving (Eq, Show)
data Media = Media { id :: MediaIdentifier
                   , mediaClass :: MediaClass
                   }

data Command = AddMedia MediaIdentifier
             | DeleteMedia MediaIdentifier

data Event = MediaWasAdded AggregateId MediaIdentifier MediaClass
           | MediaWasDeleted AggregateId
           deriving (Eq, Show)

-- For now, just return Photo
getMediaClassForFile :: MediaIdentifier -> MediaClass
getMediaClassForFile _ = Photo

mediaHandler :: Command -> IO [Event]
mediaHandler (AddMedia mediaId) = pure [MediaWasAdded id mediaId mediaClass]
  where id = hashToHexString mediaId -- the aggregate ID will simply be the sha1 hash of the MediaIdentifier
        hashToHexString = Data.List.concatMap (printf "%02x") . BS.unpack . SHA1.hash . BS.pack
        mediaClass = getMediaClassForFile mediaId
mediaHandler (DeleteMedia mediaId) = pure [MediaWasDeleted mediaId]
