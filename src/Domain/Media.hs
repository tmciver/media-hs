module Domain.Media
       ( AggregateId
       , Aggregate
       , MediaClass(..)
       , Media
       , Event(..)
       , addMediaHandler
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
data MediaClass = Photo | Video | Audio
                deriving (Eq, Show)
data Media = Media { filePath :: FilePath
                   , mediaClass :: MediaClass
                   }

data Event = MediaWasAdded AggregateId FilePath MediaClass
           | MediaWasDeleted AggregateId
           deriving (Eq, Show)

-- For now, just return Photo
getMediaClassForFile :: FilePath -> MediaClass
getMediaClassForFile path = Photo

addMediaHandler :: FilePath -> [Event]
addMediaHandler path = [MediaWasAdded id path mediaClass]
  where id = hashToHexString path -- the aggregate ID will simply be the sha1 hash of the file path
        hashToHexString = Data.List.concatMap (printf "%02x") . BS.unpack . SHA1.hash . BS.pack
        mediaClass = getMediaClassForFile path
