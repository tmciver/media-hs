{-# LANGUAGE TypeFamilies #-}

module EventSourcing.Entity where

import Control.Monad (foldM, forM_)
import Prelude hiding (init)

class Entity e where
  type EntityId e :: *
  data Command e :: *
  data Event e :: *

  entityId :: e -> EntityId e
  init :: e
  apply :: e -> Event e -> Either String e

-- In the future this may not be needed. I need to figure out how
-- to statically guarantee that has a filed for the associated
-- entity ID.
data EventList e = EventList (EntityId e) [Event e]

--type GetEvents e = EntityId e -> IO (EventList e)
--type SaveEvents e = EventList e -> IO (Either String ())
type GetEntity e = EntityId e -> IO (Either String e)
type CommandHandler e = GetEntity e -> Command e -> IO (Either String (EventList e))
type EventListener e = Event e -> IO ()

data EventStore e = EventStore
                    { get :: EntityId e -> IO (EventList e)
                    , save :: (EventList e) -> IO (Either String ())
                    }

getEntityById :: Entity e => (EntityId e -> IO (EventList e)) -> EntityId e -> IO (Either String e)
getEntityById getEvents id' = do -- IO
  (EventList _ events) <- getEvents id'
  let eitherEntity = case events of
        [] -> Left "No events for entity ID"
        events' -> foldM apply init events'
  return eitherEntity

handleCommand :: Entity e => EventStore e -> CommandHandler e -> EventListener e -> Command e -> IO (Either String (EntityId e))
handleCommand store handler listener command = do
  eitherEvents <- (handler (getEntityById (get store))) command
  case eitherEvents of
    Right(eventList@(EventList id' events)) -> do
      _ <- forM_ events listener
      _ <- save store eventList
      return $ Right id'
    Left(err) -> pure $ Left err
