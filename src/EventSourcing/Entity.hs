{-# LANGUAGE TypeFamilies #-}

module EventSourcing.Entity where

type EntityId = String

class Entity e where
  data Command e :: *
  data Event e :: *
  entityId :: e -> Maybe EntityId
  init :: e
  apply :: e -> Event e -> Either String e
  handle :: e -> Command e -> IO (Either String [Event e])
