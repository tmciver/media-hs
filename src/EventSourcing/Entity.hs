{-# LANGUAGE TypeFamilies #-}

module EventSourcing.Entity where

class Entity e where
  data Command e :: *
  data Event e :: *
  data EntityId e :: *

  entityId :: e -> EntityId e
  init :: e
  apply :: e -> Event e -> Either String e
  handle :: e -> Command e -> IO (Either String [Event e])
