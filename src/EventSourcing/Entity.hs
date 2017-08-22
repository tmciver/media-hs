{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}

module EventSourcing.Entity where

import Control.Monad (foldM)
import Prelude hiding (init)

class Entity e where
  data Command e :: *
  data Event e :: *
  data EntityId e :: *

  entityId :: e -> EntityId e
  init :: e
  apply :: e -> Event e -> Either String e
  handle :: e -> Command e -> IO (Either String [Event e])

data EventStore e = EventStore
                    { get :: EntityId e -> IO [Event e]
                    , save :: EntityId e -> [Event e] -> IO (Either String ())
                    }

-- type CommandHandler = Entity e => Command e -> IO (Either String [Event e])

getEntityById :: Entity e => EventStore e -> EntityId e -> IO (Either String e)
getEntityById store id' =
  do -- IO
    events <- get store id'
    let eitherEntity = case events of
          [] -> Left "No events for entity ID"
          events' -> foldM apply init events'
    return eitherEntity

-- handleCommand :: Entity e => Command e -> IO (Either String ())
-- handleCommand = undefined
