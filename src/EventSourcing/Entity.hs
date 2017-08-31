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
  handle :: e -> Command e -> IO (Either String [Event e])

data EventStore e = EventStore
                    { get :: EntityId e -> IO [Event e]
                    , save :: EntityId e -> [Event e] -> IO (Either String ())
                    }

type CommandHandler e = EventStore e -> Command e -> IO (Either String [Event e])
type EventListener e = Event e -> IO ()

getEntityById :: Entity e => EventStore e -> EntityId e -> IO (Either String e)
getEntityById store id' =
  do -- IO
    events <- get store id'
    let eitherEntity = case events of
          [] -> Left "No events for entity ID"
          events' -> foldM apply init events'
    return eitherEntity

handleCommand :: Entity e => EventStore e -> CommandHandler e -> EventListener e -> EntityId e -> Command e -> IO (Either String ())
handleCommand store handler listener eId command = do
  eitherEvents <- handler store command
  case eitherEvents of
    Right(events) -> do
      _ <- forM_ events listener
      save store eId events
    Left(err) -> pure $ Left err
