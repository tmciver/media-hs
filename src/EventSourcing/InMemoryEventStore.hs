module EventSourcing.InMemoryEventStore (inMemoryEventStore) where

import EventSourcing.Entity
import Data.IORef (newIORef)
import Data.Map as Map
import Data.IORef (IORef, newIORef, readIORef)
import Data.Maybe (fromMaybe)

inMemoryEventStore :: Entity e => EventStore e
inMemoryEventStore = undefined
  -- let
  -- ioref = newIORef (Map.empty :: Entity e => Map (EntityId e) [Event e])
  -- in
  --  EventStore { get = undefined --myGet ioref
  --             , save = mySave
  --             }

--type MapKey = 

-- myGet :: (Entity e) => IO (IORef (Map.Map (EntityId e) [Event e])) -> EntityId e -> IO [Event e]
-- myGet ioref id' = do
--   ref <- ioref
--   m <- readIORef ref
--   let events = fromMaybe [] (Map.lookup id' m)
--   return events

mySave :: Entity e => EntityId e -> [Event e] -> IO (Either String ())
mySave _ _ = undefined
