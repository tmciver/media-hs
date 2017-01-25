module EventSourcing.Entity where

-- class Entity id event where
--   entityId :: id

--   apply :: event -> Entity id

type EntityId = String
-- data Entity entity command event = Entity { entityId :: forall a . a
--                                  , apply :: event -> Entity entity event
--                                  }

data Entity e = Entity { _entityId :: e -> EntityId
--                       , _apply :: forall event error . event -> e -> Either error e
                       }

getEntityId :: Entity e -> e -> EntityId
getEntityId entityI entity = _entityId entityI entity

-- applyEvent :: Entity e -> event -> e -> Either error e
-- applyEvent entityI event entity = apply entityI event entity
